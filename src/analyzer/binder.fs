module Wrattler.Binder
open Wrattler.Ast

// ------------------------------------------------------------------------------------------------

//let rservice = "http://wrattler-r-service.azurewebsites.net"
let rservice = "http://localhost:7101"

open Wrattler.Common

type RFrame = 
  { name : string 
    url : string }

type RInput = 
  { code : string 
    hash : string
    frames : RFrame[] }

type RExportsType = 
  { kind : string }

type RExportsTypeFrame = 
  { kind : string 
    columns : string[][] }

type RExportsVar =   
  { variable : string
    ``type`` : RExportsType }

let evalRCode hash (frames:seq<Name * string>) code = async {
  let req = { code = code; hash = hash; frames = [| for n, f in frames -> { name = n; url = f } |] }
  let! json = Http.Request("POST", rservice + "/eval", jsonStringify req)
  let vars = unbox<RFrame[]> (jsonParse json)
  return Frames(Map.ofList [ for v in vars -> v.name, v.url ]) }
  
let getExports hash (frames:seq<Name * string>) ent = async {
  match ent with 
  | { Kind = Code("r", code, _) } ->    
      let req = { code = code; hash = hash; frames = [| for n, f in frames -> { name = n; url = f } |] }
      let! json = Http.Request("POST", rservice + "/exports", jsonStringify req)
      let vars = unbox<RExportsVar[]> (jsonParse json)
      return [ for v in vars -> v.variable ]
  | _ -> return [] }

// ------------------------------------------------------------------------------------------------

open Wrattler.Ast.AstOps

/// Represents case of the EntityKind union
type EntityCode = string

/// As we bind, we keep root entity, current scope & variables in scope
type BindingContext = 
  { //GlobalValues : Map<Name, Entity>
    Languages : Map<string, BindingContext -> CustomBlockKind -> Entity * list<string * Entity>>
    Frames : Map<Name, Entity>  
    Root : Entity
    
    Evaluate : Entity -> Async<unit>

    /// Table with previously created entities. This is a mutable mapping from 
    /// list of symbols (antecedent entities) together with entity kind & name
    /// to the actual entity. Antecedents capture dependencies (if dependency 
    /// changed, we need to recreate the entity that depends on them)
    Table : ListDictionary<Symbol, Map<EntityCode, Entity>> 
    /// Collects all bound entities and their ranges
    Bound : ResizeArray<Range * Entity> }

/// Represents result of binding syntax tree to entities 
/// (provides access to all bound entities & children lookup function)
type BindingResult(ents:(Range * Entity)[]) = 
  let childrenLookup = 
    let res = System.Collections.Generic.Dictionary<Symbol, ResizeArray<Entity>>()
    let add a e = 
      if not (res.ContainsKey(a)) then res.Add(a, ResizeArray())
      res.[a].Add(e)
    for _, e in ents do
      for a in e.Antecedents do
        add a.Symbol e
    res 
  member x.Entities = ents
  member x.GetChildren(ent:Entity) = 
    match childrenLookup.TryGetValue(ent.Symbol) with true, res -> res.ToArray() | _ -> [||]

/// Lookup entity (if it can be reused) or create & cache a new one
let bindEntity ctx lang kind =
  let antecedents, code = entityCodeAndAntecedents kind
  let symbols = ctx.Root::antecedents |> List.map (fun a -> a.Symbol)
  let nestedDict = 
    match ListDictionary.tryFind symbols ctx.Table with
    | None -> Map.empty
    | Some res -> res
  if nestedDict.ContainsKey code then 
    Log.trace("binder", "Cached %s binding: %s", lang, formatEntityKind kind)
    nestedDict.[code]
  else
    Log.trace("binder", "New %s binding: %s", lang, formatEntityKind kind)
    let full = code + String.concat "\n" [ for a in antecedents -> a.Symbol.ToString() ]
    let symbol = createSymbol (getHashCode full)
    let entity = { Language = lang; Kind = kind; Symbol = symbol; Value = None; Errors = []; Type = None; Meta = [] }
    ListDictionary.set symbols (Map.add (code) entity nestedDict) ctx.Table
    entity    

/// Assign entity to a node in parse tree
let setEntity ctx node entity = 
  ctx.Bound.Add({Start=12345;End=12345}, entity) // TODO: node.Range
  node.Entity <- Some entity
  entity

let bindNode ctx node = async {
  match node.Node.BlockKind with 
  | MarkdownBlock _ -> 
      return ctx, []

  | CustomBlock(block) ->
      let blockEnt, vars = ctx.Languages.[block.Language] ctx block 
      let blockEnt = blockEnt |> setEntity ctx node
      let frames = vars |> List.fold (fun frames (v, ent) -> Map.add v ent frames) ctx.Frames
      return { ctx with Frames = frames }, [blockEnt]

  | CodeBlock(lang, code) ->
      let vars = Map.toList ctx.Frames |> List.map snd
      let codeEnt = bindEntity ctx lang (Code(lang, code, vars))

      Log.trace("binder", " -> Known variables: %s", String.concat "," (Seq.map fst (Map.toSeq ctx.Frames)))
      let frames = ResizeArray<_>()
      for v, f in Map.toSeq ctx.Frames do
        do! ctx.Evaluate f
        match f.Value with 
        | Some(Frame data) -> frames.Add(v, data)
        | _ -> ()
      let! vars = async {
        try
          let! vars = getExports (codeEnt.Symbol.ToString()) frames codeEnt // TODO: This should not call R repeatedly
          Log.trace("binder", " -> Exporting variables: %s", String.concat "," vars)
          return vars
        with e -> 
          Log.error("binder", "Getting R exports failed: %s", e)
          return [] }

      let vars = vars |> List.map (fun v -> v, bindEntity ctx lang (DataFrame(v, codeEnt)))
      let frames = vars |> List.fold (fun frames (v, ent) -> Map.add v ent frames) ctx.Frames
      let blockEnt = bindEntity ctx lang (EntityKind.CodeBlock(lang, codeEnt, List.map snd vars)) |> setEntity ctx node
      return { ctx with Frames = frames }, [blockEnt] }

// ------------------------------------------------------------------------------------------------

/// Create a new binding context - this stores cached entities
let createContext langs ev =
  let root = { Language = "system"; Kind = EntityKind.Root; Symbol = createSymbol "root"; Value = None; Errors = []; Type = None; Meta = [] }
  { Table = System.Collections.Generic.Dictionary<_, _>(); 
    Bound = ResizeArray<_>(); Frames = Map.empty; 
    Evaluate = ev
    Languages = langs
    //GlobalValues = Map.ofList [ for e in globals -> { Name = e.Name }, e ]
    Root = root }

let bind ctx nodes = 
  ctx.Bound.Clear()
  let rec loop ctx acc nodes = async {
    match nodes with 
    | [] -> return List.rev acc
    | node::nodes ->
        let! ctx, bound = bindNode ctx node
        return! loop ctx (bound @ acc) nodes }
  
  async { 
    let! bound = loop ctx [] nodes
    let nbEnt = bindEntity ctx "system" (Notebook(List.ofSeq bound)) 
    return nbEnt, BindingResult(ctx.Bound.ToArray()) }


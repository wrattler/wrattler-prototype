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
open Wrattler.Languages

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
  ctx.Bound.Add(node.Range, entity) 
  node.Entity <- Some entity
  entity

let bindNode ctx (node:Node<_>) = async {
  let block = node.Node.BlockKind 
  let! blockEnt, vars = ctx.Languages.[block.Language].Bind(ctx, block)
  let blockEnt = blockEnt |> setEntity ctx node
  let frames = vars |> List.fold (fun frames (v, ent) -> Map.add v ent frames) ctx.Frames
  return { ctx with Frames = frames }, blockEnt }

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
        bound.Errors <- node.Node.Errors @ bound.Errors
        return! loop ctx (bound :: acc) nodes }
  
  async { 
    let! bound = loop ctx [] nodes
    let nbEnt = bindEntity ctx "system" (Notebook(List.ofSeq bound)) 
    return nbEnt, BindingResult(ctx.Bound.ToArray()) }


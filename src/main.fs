module Wrattler.Main

open Fable.Core
open Fable.Helpers
open Fable.Import

let demo = """
# Sample notebook
This is a sample notebook

```r
data <- iris
agg <- aggregate(Petal.Width~Species, data, mean)
```

## visualization

```javascript
foo
```
"""

// ------------------------------------------------------------------------------------------------

type Value = 
  | Frame of obj[]
  | Frames of Map<string, obj[]>

type EntityKind = 
  | RCode of code:string * frames:Entity list
  | RDataFrame of var:string * rblock:Entity
  | RCodeBlock of rcode:Entity * vars:Entity list
  | JsCodeBlock of string * frames:Entity list
  | Notebook of blocks:Entity list

and Entity = 
  { Kind : EntityKind
    mutable Value : Value option }

type ParsedCode = 
  | RSource of string 
  | JsSource of string

type Block = 
  | CodeBlock of code:ParsedCode
  | MarkdownBlock of obj list

type Node<'T> = 
  { Node : 'T
    mutable Entity : Entity option }

// ------------------------------------------------------------------------------------------------

let [<Emit("Array.isArray($0)")>] isArray (o:obj) : bool = failwith "JS"
let [<Emit("(typeof($0)=='string')")>] isString (o:obj) : bool = failwith "JS"

let (|MarkdownNode|_|) name (tree:obj) = 
  if isArray tree then 
    let tree = unbox<obj[]> tree
    if tree.Length > 0 && isString tree.[0] && unbox<string> tree.[0] = name then 
      Some (List.tail (List.ofArray tree))
    else None
  else None  

let parseMarkdown tree = 
  let rec loop acc pars = seq {
    match pars with 
    | MarkdownNode "para" [MarkdownNode "inlinecode" [body]]::rest ->
        if not (List.isEmpty acc) then
          yield { Node = MarkdownBlock(List.rev acc); Entity = None }

        let body = unbox<string> body
        let start = body.IndexOfAny [| '\r'; '\n' |]
        match body.[0 .. start].Trim(), body.[start..].Trim() with 
        | "r", body -> yield { Node = CodeBlock(RSource(body)); Entity = None }
        | "javascript", body -> yield { Node = CodeBlock(JsSource(body)); Entity = None }
        | _ -> failwith "Unsupported langauge..."

        yield! loop [] rest
    | node::rest ->
        yield! loop (node::acc) rest
    | [] ->
        if not (List.isEmpty acc) then
          yield { Node = MarkdownBlock(List.rev acc); Entity = None } }

  match tree with 
  | MarkdownNode "markdown" body -> List.ofSeq (loop [] body)
  | _ -> []

// ------------------------------------------------------------------------------------------------

open Wrattler.Common

type RExportsType = 
  { kind : string }

type RExportsTypeFrame = 
  { kind : string 
    columns : string[][] }

type RExportsVar =   
  { variable : string
    ``type`` : RExportsType }

type REvalValue =
  { variable : string
    data : obj[] }

let evalRCode code = async {
  let! json = Http.Request("POST", "http://wrattler-r-service.azurewebsites.net/eval", code)
  let vars = unbox<REvalValue[]> (jsonParse json)
  return Frames(Map.ofList [ for v in vars -> v.variable, v.data ]) }
  
let getExports ent = async {
  match ent with 
  | { Kind = RCode(code, _) } ->
      let! json = Http.Request("POST", "http://wrattler-r-service.azurewebsites.net/exports", code)
      let vars = unbox<RExportsVar[]> (jsonParse json)
      return [ for v in vars -> v.variable ]
  | _ -> return [] }

let bind nodes = 
  let bindNode variables node = async {
    Log.trace("test", "Bind node: %O", box node)
    match node.Node with 
    | MarkdownBlock _ -> 
        return variables, []
    | CodeBlock(code) ->
        let vars = Map.toList variables |> List.map snd
        let kind = match code with RSource code -> RCode(code, vars) | JsSource code -> JsCodeBlock(code, vars)
        let ent = { Kind = kind; Value = None }
        let! vars = getExports ent
        let vars = vars |> List.map (fun v -> v, { Kind = RDataFrame(v, ent); Value = None })
        let variables = vars |> List.fold (fun variables (v, ent) -> Map.add v ent variables) variables
        let blockEnt = { Kind = RCodeBlock(ent, List.map snd vars); Value = None }
        node.Entity <- Some blockEnt
        return variables, [blockEnt] }

  let rec loop variables acc nodes = async {
    match nodes with 
    | [] -> return List.rev acc
    | node::nodes ->
        let! variables, bound = bindNode variables node
        return! loop variables (bound @ acc) nodes }
  
  async { 
    let! bound = loop Map.empty [] nodes
    return { Kind = Notebook(List.ofSeq bound); Value = None } }

// ------------------------------------------------------------------------------------------------

let rec evaluate ent = async {
  Log.trace("test", "Evaluating: %O", box ent)
  match ent.Kind with
  | _ when ent.Value.IsSome -> ()
  | RCode(code, _) ->
      let! res = evalRCode code
      ent.Value <- Some res
  | RDataFrame(v, rblock) ->
      do! evaluate rblock
      match rblock.Value with
      | Some(Frames frames) -> ent.Value <- Some(Frame(frames.[v]))
      | _ -> failwith "R block did not evaluate to Frames"
  | RCodeBlock(rcode, vars) ->
      do! evaluate rcode
      for v in vars do do! evaluate v
  | JsCodeBlock(code, _) ->
      ()
  | Notebook ents -> 
      for ent in ents do 
        do! evaluate ent }

// ------------------------------------------------------------------------------------------------

open Wrattler.Html

type State = 
  { Started : bool
    Nodes : Node<Block> list }

type Event = 
  | Refresh

let state =
  { Started = false
    Nodes = parseMarkdown (Markdown.markdown.parse(demo)) }

let startEvaluation trigger state = Async.StartImmediate <| async { 
  try
    Log.trace("test", "Binding: %O", Array.ofList state.Nodes)
    let! bound = bind state.Nodes
    Log.trace("test", "Evaluating...")
    do! evaluate bound
    for block in state.Nodes do
      Log.trace("test", "Block entity: %O", block.Entity)
    trigger Refresh
  with e ->
    Log.exn("test", "Failed: %O", e) }

let render trigger state = 
  if not state.Started then startEvaluation trigger state
  h?div [] [
    for nd in state.Nodes do
      match nd.Node with
      | CodeBlock(RSource src | JsSource src) ->
          yield h?textarea ["class" => "form-control"; "rows" => "6"] [ text src ]
          match nd.Entity with
          | Some { Kind = RCodeBlock(_, vars) } ->
              let vars = vars |> List.choose (function { Kind = RDataFrame(v, _) } -> Some v | _ -> None)
              yield h?h3 [] [ text (String.concat "," vars) ]
          | _ ->
              yield h?h3 [] [ text ("loading...") ]

      | MarkdownBlock objs ->        
          yield h?div [] [text (Markdown.markdown.renderJsonML(Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::objs)))) ] ]

let update state = function
  | Refresh -> { state with Started = true }

createVirtualDomApp "demo" state render update

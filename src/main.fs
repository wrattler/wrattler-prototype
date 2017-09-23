module Wrattler.Main

open Fable.Core
open Fable.Helpers
open Fable.Import

let demo = """
# Sample notebook
This is a _sample_ notebook

```r
data <- iris
agg <- aggregate(Petal.Width~Species, data, mean)
colnames(agg)[2] <- "PetalWidth"
```

## visualization

```javascript
var spec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
  "width": 600,
  "data": {
    "values": agg
  },
  "mark": "bar",
  "encoding": {
    "x": {"field": "Species", "type": "ordinal"},
    "y": {"field": "PetalWidth", "type": "quantitative"}
  }
}
addOutput(function(id) { 
  vega.embed("#" + id, spec, {actions:false});
});
```

Irrelevant [link](http://turing.ac.uk)
"""

// ------------------------------------------------------------------------------------------------

type Value = 
  | Outputs of (string -> unit)[]
  | Frame of obj[]
  | Frames of Map<string, obj[]>

type EntityKind = 
  | Code of lang:string * code:string * frames:Entity list
  | DataFrame of var:string * rblock:Entity
  | CodeBlock of lang:string * code:Entity * vars:Entity list
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
  | { Kind = Code("r", code, _) } ->
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
        let lang, code = match code with RSource code -> "r", code | JsSource code -> "js", code
        let kind = Code(lang, code, vars)
        let ent = { Kind = kind; Value = None }
        let! vars = getExports ent
        let vars = vars |> List.map (fun v -> v, { Kind = DataFrame(v, ent); Value = None })
        let variables = vars |> List.fold (fun variables (v, ent) -> Map.add v ent variables) variables
        let blockEnt = { Kind = EntityKind.CodeBlock(lang, ent, List.map snd vars); Value = None }
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
  | Code("r", code, _) ->
      let! res = evalRCode code
      ent.Value <- Some res
  | Code("js", code, vars) ->
      let vars = vars |> List.choose (function { Kind = DataFrame(n, _); Value = Some(Frame v) } -> Some(n, v) | _ -> None)
      let code = 
        "(function(addOutput) { return (function(frames) {" +
        (vars |> Seq.mapi (fun i (v, _) -> sprintf "  var %s = frames[%d];" v i) |> String.concat "\n") +
        "  " + code + "}) })"
      let frames = [| for (_, data) in vars -> box data |]
      let outputs = ResizeArray<_>()
      eval<((string -> unit) -> unit) -> obj[] -> unit> code outputs.Add frames
      ent.Value <- Some(Outputs(outputs.ToArray()))
  | Code _ -> failwith "Code in unsupported langauge"
  | DataFrame(v, rblock) ->
      do! evaluate rblock
      match rblock.Value with
      | Some(Frames frames) -> 
          Log.trace("eval", "Frame %s = %O", v, Map.find v frames)
          ent.Value <- Some(Frame(Map.find v frames))
      | _ -> failwith "R block did not evaluate to Frames"
  | EntityKind.CodeBlock(lang, rcode, vars) ->
      do! evaluate rcode
      for v in vars do do! evaluate v
  | Notebook ents -> 
      for ent in ents do 
        do! evaluate ent }

// ------------------------------------------------------------------------------------------------

open Wrattler.Html

type State = 
  { Started : bool
    Nodes : Node<Block> list
    SelectedVariables : Map<string, string> }

type Event = 
  | Refresh
  | DisplayVariable of string * string

let state =
  { Started = false
    SelectedVariables = Map.empty
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

let rec renderHtmlTree tree =
  Log.trace("render", "Rendering: %O", box tree)
  if isString tree then text(unbox tree)
  elif isArray tree then
    let arr = unbox<obj[]> tree
    let contentIdx, props = 
      if isObject arr.[1] then 
        let props = JsHelpers.properties(arr.[1])
        2, [for p in props -> p.key => unbox p.value ]
      else 1, []
    h.el(unbox arr.[0]) props [ for i in contentIdx .. arr.Length-1 -> renderHtmlTree arr.[i] ]
  else failwithf "Unexpected node: %A" tree

let renderTable objs = 
  Log.trace("render", "Table: %O", box obj)
  let first = Array.head objs
  let props = JsHelpers.properties(first)
  h?table ["class" => "table"] [
    h?thead [] [ 
      h?tr [] [
        for prop in props -> h?th [] [text prop.key]
      ]
    ]
    h?tbody [] [
      for obj in objs -> 
        h?tr [] [
          for prop in props -> h?td [] [ text(string (getProperty obj prop.key))  ]
        ]
    ]
  ]

let render trigger state = 
  if not state.Started then startEvaluation trigger state
  h?div [] [
    for nd in state.Nodes do
      match nd.Node with
      | CodeBlock(RSource src | JsSource src) ->
          yield h?textarea ["class" => "form-control"; "rows" => "6"] [ text src ]
          Log.trace("render", "Rendering output of block: %O", nd.Entity.Value)
          match nd.Entity with
          | Some { Kind = EntityKind.CodeBlock("js", { Kind = EntityKind.Code(_, code, _); Value = Some(Outputs outs) }, _) } ->
              // TODO: Use entity symbol for h.delayed
              Log.trace("render", "Rendering output of JS block...")
              for out, i in Seq.zip outs [0 .. outs.Length-1] do
                let id = sprintf "output_%d_%d" i (hash code)
                yield h.delayed id (text "") (fun id -> out id)

          | Some { Kind = EntityKind.CodeBlock(_, _, vars) } ->
              let vars = vars |> List.choose (function { Kind = DataFrame(var, _); Value = Some(Frame value) } -> Some(var, value) | _ -> None)
              if not (List.isEmpty vars) then
                let selected = defaultArg (state.SelectedVariables.TryFind(src)) (fst(List.head vars))
                yield h?ul ["class" => "nav nav-tabs"] [
                  for v, data in vars do 
                  yield h?li ["class" => "nav-item"] [
                    h?a [
                      "class" => (if v = selected then "nav-link active" else "nav-link")
                      "click" =!> fun _ _ -> trigger (DisplayVariable(src, v))
                      "href" => "#" ] [text v]
                  ]
                ]
                for v, data in vars do
                  if v = selected then 
                    Log.trace("render", "Data = %O", data)
                    yield renderTable data
          | _ ->
              yield h?p [] [ text ("loading...") ]

      | MarkdownBlock objs ->
          let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::objs)) |> unbox<obj[]>
          for i in 1 .. tree.Length-1 do yield renderHtmlTree tree.[i] ]

let update state = function
  | Refresh -> { state with Started = true }
  | DisplayVariable(k, v) -> { state with SelectedVariables = Map.add k v state.SelectedVariables }

createVirtualDomApp "demo" state render update

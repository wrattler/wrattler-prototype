module Wrattler.Main

open Wrattler.Ast
open Wrattler.Binder
open Wrattler.Common

open Fable.Core
open Fable.Helpers
open Fable.Import

let demo = """
# Sample notebook
This is a _sample_ notebook

```r
data <- iris
```

now we do some analysis

```r
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

open Wrattler.Html

type State = 
  { BindingContext : BindingContext
    Nodes : Node<Block> list
    SelectedVariables : Map<string, string> }

type Event = 
  | Refresh
  | UpdateCode of int * string
  | UpdateNodes of Node<Block> list
  | DisplayVariable of string * string

let state =
  { BindingContext = Binder.createContext Interpreter.evaluate
    SelectedVariables = Map.empty
    Nodes = parseMarkdown (Markdown.markdown.parse(demo)) }

let startEvaluation trigger state = Async.StartImmediate <| async { 
  try
    let! bound = Binder.bind state.BindingContext state.Nodes
    trigger (UpdateNodes state.Nodes)
    do! Interpreter.evaluate bound
    trigger Refresh
  with e ->
    Log.exn("main", "Failed: %O", e) }

let rec renderHtmlTree tree =
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
  h?div [] [
    for index, nd in Seq.zip [0 .. state.Nodes.Length-1] state.Nodes do
      match nd.Node with
      | CodeBlock(RSource src | JsSource src) ->
          yield h?textarea [
            "class" => "form-control"; "rows" => "6"
            "change" =!> fun el _ -> trigger(UpdateCode(index, (unbox<Browser.HTMLTextAreaElement> el).value)) ] [ text src ]
          match nd.Entity with
          | Some { Kind = EntityKind.CodeBlock("js", { Kind = EntityKind.Code(_, code, _); Value = Some(Outputs outs) }, _) } ->
              // TODO: Use entity symbol for h.delayed
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
                    yield renderTable data
          | _ ->
              yield h?p [] [ text ("loading...") ]

      | MarkdownBlock objs ->
          let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::objs)) |> unbox<obj[]>
          for i in 1 .. tree.Length-1 do yield renderHtmlTree tree.[i] ]

let update trigger state evt =
  match evt with
  | Refresh -> state
  | UpdateNodes newNodes -> { state with Nodes = newNodes }
  | UpdateCode(i, newCode) -> 
      let nodes = 
        state.Nodes 
        |> List.mapi (fun j node ->
          match node.Node with 
          | CodeBlock(RSource _) when i = j -> CodeBlock(RSource newCode)
          | CodeBlock(JsSource _) when i = j -> CodeBlock(JsSource newCode) 
          | node -> node)
        |> List.map (fun node -> { Node = node; Entity = None })
      startEvaluation trigger { state with Nodes = nodes }
      state

  | DisplayVariable(k, v) -> 
      { state with SelectedVariables = Map.add k v state.SelectedVariables }

let app = createVirtualDomApp "demo" state render update
app.Trigger(UpdateCode(-1, ""))

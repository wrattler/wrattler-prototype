module Wrattler.Main
#nowarn "40"

open Wrattler.Ast
open Wrattler.Binder
open Wrattler.Common

open Fable.Core
open Fable.Import

let demo = """
# Sample data analysis

This is a sample notebook showing the current state of the project. It has some support for
_polyglot programming_ - you can combine some R blocks to do data analysis with some 
JavaScript blocks to do visualizations

### Testing The Gamma

```gamma
let test = 40 + "hello"
```

### Analysing data using R

First we get and clean some data. This is trivial for now, but the important point is that
we define a frame `data` that we will access later.

```r
data <- iris
```

Now that we have data, we can do some analysis. Note that the `data` frame is passed from 
one block to another. Now we use it and define another data frame `agg` with some summary:

```r
agg <- aggregate(Petal.Width~Species, data, mean)
colnames(agg)[2] <- "PetalWidth"
```

### Visualizing data using Vega

Now the fun part. The `agg` frame is also available to the following JavaScript block,
which mens that we can easily create client-side visualizations! The `agg` frame is 
represented as an array of objects (rows), which works nicely with Vega:

```javascript
var spec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
  "width": 600,
  "data": { "values": agg },
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

Just to prove that this Markdown parser works, here is alos a link to the [Alan Turing
Institute website](http://turing.ac.uk), which you'll, no doubt, find very interesting!
"""

// ------------------------------------------------------------------------------------------------
open Wrattler.Languages
open Wrattler.Ast.AstOps

let recursiveAnalyzer result = 
  { new Analyzer<_, _, _> with 
      member x.CreateContext(_) = null
      member x.Analyze(ent, ctx) = async { 
        for ant in ent.Antecedents do 
          do! ctx.Analyze(ant, ctx.Context) 
        return result } }

let defaultTypeChecker = recursiveAnalyzer { new Type } 
let defaultInterpreter = recursiveAnalyzer Value.Nothing

let builtinInterprter evalf = 
  { new Analyzer<unit, Wrattler.Ast.Value, _> with
      member x.CreateContext(_) = null
      member x.Analyze(ent, ctx) = evalf ctx ent }

let rlang = 
  { new LanguagePlugin<obj, obj> with
      member x.Bind(_, _) = failwith "R binder"
      member x.TypeChecker = None
      member x.Interpreter = Some(builtinInterprter Interpreter.evalR)
      member x.Parse(code) = 
        CodeBlock("r", code), [] }

let jslang = 
  { new LanguagePlugin<obj, obj> with
      member x.Bind(_, _) = failwith "JS binder"
      member x.Interpreter = Some(builtinInterprter Interpreter.evalJs)
      member x.TypeChecker = None
      member x.Parse(code) = 
        CodeBlock("javascript", code), [] }

let syslang = 
  { new LanguagePlugin<obj, obj> with
      member x.Bind(_, _) = failwith "sys binder"
      member x.TypeChecker = None
      member x.Interpreter = None
      member x.Parse(code) = failwith "sys parser" }

let languages : Map<string, LanguagePlugin<obj, obj>> = 
  [ "system", syslang
    "r", rlang
    "javascript", jslang
    "gamma", unbox Gamma.Plugin.language ] |> Map.ofSeq
 
let rec createAnalyzerContext kind checker (getAnalyzer:_ -> Analyzer<_, _, _>) setResult input (gctx:Lazy<_>) langName (lang:LanguagePlugin<obj, obj>) = 
  { new AnalyzerContext<obj> with
        member x.GlobalContext = gctx.Value
        member x.Context = getAnalyzer(lang).CreateContext(input)
        member x.Analyze(ent, ctx) = async {
          if not (checker ent) then 
            Log.trace(kind, "Entity: %s", AstOps.formatEntityKind ent.Kind)
            // TODO: DO not create new contexts if it has not changed
            let rec ntctx = createAnalyzerContext kind checker getAnalyzer setResult input (lazy ngctx) langName lang
            and ngctx = { gctx.Value with Contexts = gctx.Value.Contexts.Add(langName, ntctx) }
            let ctx = gctx.Value.Contexts.[ent.Language]
            let errorCount = ctx.GlobalContext.Errors.Count
            let! res = getAnalyzer(languages.[ent.Language]).Analyze(ent, ctx) 
            ent.Errors <- ent.Errors @ [ for i in errorCount .. ctx.GlobalContext.Errors.Count - 1 -> ctx.GlobalContext.Errors.[i] ]
            Log.trace(kind, "Entity %s, result:  %O", AstOps.formatEntityKind ent.Kind, res)
            setResult ent res } }

let createCheckingContext = 
  createAnalyzerContext "typechecker"
    (fun ent -> ent.Type.IsSome)
    (fun lang -> defaultArg lang.TypeChecker defaultTypeChecker)
    (fun ent typ -> ent.Type <- Some typ)

let createInterpreterContext = 
  createAnalyzerContext "interpreter"
    (fun ent -> ent.Value.IsSome)
    (fun lang -> defaultArg lang.Interpreter defaultInterpreter)
    (fun ent res -> ent.Value <- Some res)

let startAnalyzer createContext (ent:Entity) = async {
  let rec gctx = 
    let langContexts = languages |> Map.map (createContext (lazy gctx))
    GlobalAnalyzerContext.Create(langContexts)
  do! gctx.Contexts.[ent.Language].Analyze(ent, gctx.Contexts.[ent.Language])
  Log.trace("typechecker", "Errors: %O", gctx.Errors.ToArray()) }

// ------------------------------------------------------------------------------------------------

let (|MarkdownNode|_|) name (tree:obj) = 
  if isArray tree then 
    let tree = unbox<obj[]> tree
    if tree.Length > 0 && isString tree.[0] && unbox<string> tree.[0] = name then 
      Some (List.tail (List.ofArray tree))
    else None
  else None  

let blockId = 
  let mutable count = 0 
  fun () -> count <- count + 1; sprintf "block_%d" count

let parseMarkdown tree = 
  let rec loop acc pars = seq {
    match pars with 
    | MarkdownNode "para" [MarkdownNode "inlinecode" [body]]::rest ->
        if not (List.isEmpty acc) then
          yield { Node = { Symbol = blockId(); BlockKind = MarkdownBlock(List.rev acc); Errors = [] }; Entity = None; Range = { Start = 0; End = 0; } }

        let body = unbox<string> body
        let start = body.IndexOfAny [| '\r'; '\n' |]
        let lang, body = body.[0 .. start].Trim(), body.[start..].Trim() 
        match languages.TryFind(lang) with
        | Some lang ->
            let block, errors = lang.Parse(body)
            yield { Node = { Symbol = blockId(); BlockKind = block; Errors = errors }; Entity = None; Range = { Start = 0; End = body.Length } } 
        | _ ->
            failwithf "Unsupported language '%s'" lang

        yield! loop [] rest
    | node::rest ->
        yield! loop (node::acc) rest
    | [] ->
        if not (List.isEmpty acc) then
          yield { Node = { Symbol = blockId(); BlockKind = MarkdownBlock(List.rev acc); Errors = [] }; Entity = None; Range = { Start = 0; End = 0 } } }

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
  let binders = languages |> Map.map (fun _ v a b -> v.Bind(a, b))
  { BindingContext = Binder.createContext binders (startAnalyzer (createInterpreterContext ()))
    SelectedVariables = Map.empty
    Nodes = parseMarkdown (Markdown.markdown.parse(demo)) }

let startEvaluation trigger state = Async.StartImmediate <| async { 
  try
    let! bound, bindingResult = Binder.bind state.BindingContext state.Nodes
    trigger (UpdateNodes state.Nodes)
    do! startAnalyzer (createCheckingContext bindingResult) bound
    //let evals = languages |> Map.map (fun _ l -> l.Evaluate)
    //do! Interpreter.evaluate evals bound
    do! startAnalyzer (createInterpreterContext ()) bound    
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

let renderTable url trigger = 
  match Datastore.tryFetchPreview url trigger with 
  | None ->
      h?div ["class" => "preview"] [ h?p [] [text "Loading..."] ]
  | Some objs ->
      let first = Array.head objs
      let props = JsHelpers.properties(first)
      h?div ["class" => "preview"] [
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
      ]

open Fable.Import.Monaco

let createMonacoEditor id lang code = 
  let services = JsInterop.createEmpty<editor.IEditorOverrideServices>
  let options = JsInterop.createEmpty<editor.IEditorConstructionOptions>
  let scroll = JsInterop.createEmpty<editor.IEditorScrollbarOptions>
  let noMini = JsInterop.createEmpty<editor.IEditorMinimapOptions>
  noMini.enabled <- Some false
  scroll.vertical <- Some "none"
  scroll.horizontal <- Some "auto"
  options.scrollbar <- Some scroll
  options.value <- Some code
  options.language <- Some lang
  options.lineNumbersMinChars <- Some 3.0
  options.contextmenu <- Some false
  options.scrollBeyondLastLine <- Some false
  options.overviewRulerLanes <- Some 0.0
  options.fontSize <- Some 14.0
  options.minimap <- Some noMini
  options.lineHeight <- Some 20.0
  options.fontFamily <- Some "Monaco"
  options.lineNumbers <- Some (box false)
  let el = Browser.document.getElementById(id)
  let ed = editor.Globals.create(el, options, services)

  let mutable lastHeight = -1.0
  let maxHeight = 500.0
  let autosizeEditor () =
    let text = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
    let lines = 1.0 + float (text.Split('\n').Length)
    let zoneHeight = 0.0 //match previewService with Some ps -> ps.ZoneHeight | _ -> 0.0
    let height = min maxHeight (max 20.0 (lines * 20.0 + zoneHeight))
    if height <> lastHeight then
      lastHeight <- height
      let dim = JsInterop.createEmpty<editor.IDimension>
      dim.width <- el.clientWidth
      dim.height <- height
      ed.layout(dim)
      el.style.height <- string dim.height + "px" 
      el.style.width <- string dim.width + "px" 

  ed.getModel().onDidChangeContent(fun _ -> autosizeEditor ()) |> ignore     
  autosizeEditor ()
  ed

let render trigger state = 
  h.stable [
    for index, nd in Seq.zip [0 .. state.Nodes.Length-1] state.Nodes do
      match nd.Node with
      | { Symbol = sym; BlockKind = CustomBlock(block) } ->
          yield sym, h?div [] [ 
            match nd.Entity with 
            | Some ent -> yield h?p [] [ text (sprintf "Custom: Type: %A, Errors: %A" ent.Type ent.Errors) ]
            | None -> yield h?p [] [ text ("Custom: No entity") ]
          ]

      | { Symbol = sym; BlockKind = CodeBlock(lang, src) } ->
          yield sym, h.custom 
            (fun elid ->
              h?div ["class" => "block-input"] [
                h?div ["class" => "tools"] [
                  h?a ["href" => "javascript:;"] [ h?i ["class" => "fa fa-code"] []; text "hide source" ] 
                ]
                h?div ["id" => elid + "_editor" ] []
              ] |> renderTo (Browser.document.getElementById(elid))

              let ed = createMonacoEditor (elid + "_editor") lang src 
              ed.onKeyDown(fun ke -> 
                if ke.altKey && ke.keyCode = KeyCode.Enter then 
                  trigger(UpdateCode(index, ed.getModel().getValue())) ) |> ignore
              ed )
            (fun ed -> () )

          yield sym + "-output", h?div ["class" => "block-output"] [
            match nd.Entity with
            | Some { Kind = EntityKind.CodeBlock("javascript", { Kind = EntityKind.Code(_, code, _); Value = Some(Outputs outs) }, _) } ->
                // TODO: Use entity symbol for h.delayed
                for out, i in Seq.zip outs [0 .. outs.Length-1] do
                  let id = sprintf "output_%d_%d" i (hash code)
                  yield h.delayed id (text "") (fun id -> out id)

            | Some { Kind = EntityKind.CodeBlock(_, _, vars) } ->
                let vars = vars |> List.choose (function { Kind = DataFrame(var, _); Value = Some(Frame value) } -> Some(var, value) | _ -> None)
                if not (List.isEmpty vars) then
                  let selected = defaultArg (state.SelectedVariables.TryFind(src)) (fst(List.head vars))
                  yield h?ul ["class" => "nav nav-pills"] [
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
                      yield renderTable data (fun _ -> trigger Refresh)
            | _ ->
                yield h?p [] [ text ("loading...") ]
          ]

      | { Symbol = sym; BlockKind = MarkdownBlock objs } ->
          yield sym, h?div ["class" => "block-markdown"] [
            yield h?div ["class" => "tools"] [
                h?a ["href" => "javascript:;"] [ h?i ["class" => "fa fa-code"] []; text "edit source" ] 
              ]
            let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::objs)) |> unbox<obj[]>
            for i in 1 .. tree.Length-1 do yield renderHtmlTree tree.[i] ] ]

let update trigger state evt =
  match evt with
  | Refresh -> state
  | UpdateNodes newNodes -> { state with Nodes = newNodes }
  | UpdateCode(i, newCode) -> 
      let nodes = 
        state.Nodes 
        |> List.mapi (fun j node ->
          match node.Node.BlockKind with 
          | CodeBlock(lang, _) when i = j -> node.Range, { node.Node with BlockKind = CodeBlock(lang, newCode) }
          | _ -> node.Range, node.Node)
        |> List.map (fun (rng, node) -> { Node = node; Range = rng; Entity = None })
      startEvaluation trigger { state with Nodes = nodes }
      state

  | DisplayVariable(k, v) -> 
      { state with SelectedVariables = Map.add k v state.SelectedVariables }

let app = createVirtualDomApp "demo" state render update
app.Trigger(UpdateCode(-1, ""))


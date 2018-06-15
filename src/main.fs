module Wrattler.Main
#nowarn "40"

open Wrattler.Ast
open Wrattler.Binder
open Wrattler.Common

open Fable.Core
open Fable.Import

open Wrattler.Languages
open Wrattler.Ast.AstOps
(*
let demo = """
# Sample data analysis

This is a sample notebook showing the current state of the project. It has some support for
_polyglot programming_ - you can combine some R blocks to do data analysis with some 
JavaScript blocks to do visualizations

### Testing The Gamma

```gamma
let bb2014 = 
  web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls")
    .explore.'drop columns'.'drop Id'.'drop Distance band'.'drop Distance band used for weighting'
    .'drop DNS failure (%)24-hour'.'drop DNS failure (%)8-10pm weekday'.'drop DNS resolution (ms)24-hour'
    .'drop DNS resolution (ms)8-10pm weekday'.'drop Download speed (Mbit/s) 8-10pm weekday'.'drop Download speed (Mbit/s) Max'
    .'drop Headline speed'.'drop ISP'.'drop isp weights'.'drop Jitter down (ms)24-hour'.'drop Jitter down (ms)8-10pm weekday'
    .'drop Jitter up (ms)24-hour'.'drop Jitter up (ms)8-10pm weekday'.'drop Latency (ms)8-10pm weekday'.'drop LLU'
    .'drop Market'.'drop nat weights'.'drop Packet loss (%)24-hour'.'drop Packet loss (%)8-10pm weekday'.'drop Technology'
    .'drop Upload speed (Mbit/s)8-10pm weekday'.'drop Upload speed (Mbit/s)Max'.'drop Web page (ms)8-10pm weekday'
    .then.'get the data'
let bb2015 = 
  web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0015/50073/panellist-data.csv.xls")
    .explore.'get the data'
```

Yoi    

```gamma
datadiff.adapt(bb2015, bb2014)
```

let teams = 
  olympics
    .'group data'.'by Year'.'count distinct Team'.then
    .'sort data'.'by Year'.then
    .'get series'.'with key Year'.'and value Team'
compost.charts.line(teams)
  .setAxisX(minValue=1896, maxValue=2020)
  .setTitle("Number of disticnt teams in the Olympics")

### Analysing data using R

Nothing:

```r
1+1
```

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
*)

// ------------------------------------------------------------------------------------------------

let bindRCodeBlock ctx (cb:CodeBlock) = async {
  Log.trace("binder", "Known variables (r): %s", String.concat "," (Seq.map fst (Map.toSeq ctx.Frames)))
  let frames = ctx.Frames |> Seq.map (fun (KeyValue(k, _)) -> k) 
  let! imports, exports = async {
    try return! getExports cb.Code (getHashCode cb.Code) frames 
    with e -> 
      Log.error("binder", "Getting R exports failed: %s", e)
      return [], [] }

  let importVars = ctx.Frames |> Seq.choose (fun (KeyValue(k, v)) ->
    if Seq.exists ((=) k) imports then Some(k, v) else None) |> List.ofSeq
  let codeEnt = bindEntity ctx cb.Language (Code(cb.Language, cb.Code, List.map snd importVars))
  let exportVars = exports |> List.map (fun v -> v, bindEntity ctx cb.Language (DataFrame(v, codeEnt)))
  Log.trace("binder", "Importing variables (r): %s", String.concat "," (List.map fst importVars))
  Log.trace("binder", "Exporting variables (r): %s", String.concat "," (List.map fst exportVars))

  let blockEnt = bindEntity ctx cb.Language (EntityKind.CodeBlock(cb.Language, codeEnt, List.map snd exportVars)) 
  return blockEnt, exportVars }

let bindJsCodeBlock ctx (cb:CodeBlock) = async {
  // Guesswork - but only depend on in-scope variable that appear in code
  Log.trace("binder", "Known variables (js): %s", String.concat "," (Seq.map fst (Map.toSeq ctx.Frames)))
  let vars = ctx.Frames |> Seq.choose (fun (KeyValue(name, ent)) -> if cb.Code.Contains name then Some(name, ent) else None)
  Log.trace("binder", "Importing variables (js): %s", String.concat "," (Seq.map fst vars))
  let codeEnt = bindEntity ctx cb.Language (Code(cb.Language, cb.Code, List.ofSeq (Seq.map snd vars)))  
  let blockEnt = bindEntity ctx cb.Language (EntityKind.CodeBlock(cb.Language, codeEnt, [])) 
  return blockEnt, [] }

let recursiveAnalyzer result = 
  { new Analyzer<_, _, _> with 
      member x.CreateContext(_) = async.Return null
      member x.Analyze(ent, ctx) = async { 
        for ant in ent.Antecedents do 
          do! ctx.Analyze(ant, ctx.Context) 
        return result } }

let defaultTypeChecker = recursiveAnalyzer { new Type } 
let defaultInterpreter = recursiveAnalyzer ("", Value.Nothing)

let builtinInterprter evalf = 
  { new Analyzer<unit, string * Wrattler.Ast.Value, _> with
      member x.CreateContext(_) = async.Return null
      member x.Analyze(ent, ctx) = evalf ctx ent }

type SimpleCodeBlock(lang, code) = 
  interface BlockKind with
    member x.Language = lang
  interface CodeBlock with
    member x.Code = code
    member x.WithCode(newCode) = SimpleCodeBlock(lang, newCode) :> _, []

open Wrattler.Html

let renderJsEntity ctx state entity = 
  [ match entity with
    | { Kind = EntityKind.CodeBlock("javascript", { Kind = EntityKind.Code(_, code, _); Value = Some(Outputs outs) }, _) } ->
        // TODO: Use entity symbol for h.delayed
        for out, i in Seq.zip outs [0 .. outs.Length-1] do
          let id = sprintf "output_%d_%d" i (hash code)
          yield (text "") |> h.once id (fun el -> out el.id) 
    | _ -> 
        yield h?ppp [] [ text (sprintf "No JS. Entity: %A" entity) ] ]

let renderFrames (ctx:EditorContext<_>) (state:Rendering.StandardEditorState) entity =
  [ match entity with 
    | { Kind = EntityKind.CodeBlock(_, { Console = out }, vars) } ->
        let vars = vars |> List.choose (function { Kind = DataFrame(var, _); Value = Some(Frame value) } -> Some(var, value) | _ -> None)
        let vars = match out with Some out -> ("(console)", out)::vars | _ -> vars
        if not (List.isEmpty vars) then
          let selected = defaultArg state.SelectedVariable (fst(List.head vars))
          yield h?ul ["class" => "nav nav-pills"] [
            for v, data in vars do 
            yield h?li ["class" => "nav-item"] [
              h?a [
                "class" => (if v = selected then "nav-link active" else "nav-link")
                "click" =!> fun _ _ -> ctx.Trigger(Rendering.DisplayVariable(v))
                "href" => "javascript:;" ] [h?i ["class"=>"fa fa-table"; "style"=>"margin-right:5px"][]; text v]
            ]
          ]
          for v, data in vars do
            if v = selected then 
              if v = "(console)" then
                yield h?pre [] [text data]
              else yield Rendering.renderTable data ctx.Refresh
    | ent ->
        yield h?p [] [ text (sprintf "No frames. Entity: %A" ent) ] ]


let rlang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(ctx, nd) = bindRCodeBlock ctx (nd :?> CodeBlock)
      member x.Editor = Some(Rendering.createStandardEditor renderFrames ignore)
      member x.TypeChecker = None
      member x.Interpreter = Some(builtinInterprter Interpreter.evalR)
      member x.Parse(block, code) = SimpleCodeBlock("r", code) :> _, [] }

let jslang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(ctx, nd) = bindJsCodeBlock ctx (nd :?> CodeBlock)
      member x.Editor = Some(Rendering.createStandardEditor renderJsEntity ignore)
      member x.Interpreter = Some(builtinInterprter Interpreter.evalJs)
      member x.TypeChecker = None
      member x.Parse(block, code) = SimpleCodeBlock("javascript", code) :> _, [] }

(*

      | { Symbol = sym; BlockKind = MarkdownBlock objs } ->
          yield sym, h?div ["class" => "block-markdown"] [
            yield h?div ["class" => "tools"] [
                h?a ["href" => "javascript:;"] [ h?i ["class" => "fa fa-code"] []; text "edit source" ] 
              ]
            let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::objs)) |> unbox<obj[]>
            for i in 1 .. tree.Length-1 do yield Rendering.renderHtmlTree tree.[i] ] ]

*)

open Wrattler.Html

let mdlang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(ctx, block) = 
        match block with 
        | :? MarkdownBlock as mb -> 
            let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::mb.Parsed)) |> unbox<obj[]>
            let html = Rendering.formatHtmlTree tree
            let ck = 
              { new CustomEntityKind with 
                  member x.Language = "gamma"
                  member x.FormatEntity() = "markdown"
                  member x.GetCodeAndAntecedents() = [], sprintf "<markdown code='%s' />" html }
            let ent = bindEntity ctx "markdown" (EntityKind.CustomEntity(ck))
            async.Return(ent, [])
        | _ -> failwith "mdlang: Unexpected node"

      member x.Editor = 
        let renderView (state:Rendering.ToggleEditorState) = 
          match state.Node.Node.BlockKind with 
          | :? MarkdownBlock as mb ->
            h?div [] [
              let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::mb.Parsed)) |> unbox<obj[]>
              for i in 1 .. tree.Length-1 do yield Rendering.renderHtmlTree tree.[i] ]
          | _ -> failwith "mdlang: Unexpected node in renderView"
        Some(Rendering.createToggleEditor renderView ignore)
      member x.TypeChecker = None
      member x.Interpreter = None
      member x.Parse(block, code) = 
        let cb = { MarkdownBlock.Source = ""; Parsed = [] } :> CodeBlock
        let block, errs = cb.WithCode(code)
        block :> _, errs }

let syslang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(_, _) = failwith "sys binder"
      member x.Editor = None
      member x.TypeChecker = None
      member x.Interpreter = None
      member x.Parse(block, code) = failwith "sys parser" }

let languages : Map<string, LanguagePlugin<obj, obj, obj, obj>> = 
  [ "system", syslang
    "markdown", unbox mdlang
    "r", unbox rlang
    "javascript", unbox jslang
    "gamma", unbox Gamma.Plugin.language ] |> Map.ofSeq
 
let rec createAnalyzerContext kind checker (getAnalyzer:_ -> Analyzer<_, _, _>) setResult input (gctx:Lazy<_>) langName (lang:LanguagePlugin<obj, obj, _, _>) = 
  async.Bind(getAnalyzer(lang).CreateContext(input), fun localCtx ->
  { new AnalyzerContext<obj> with
      member x.GlobalContext = gctx.Value
      member x.Context = localCtx
      member x.Analyze(ent, ctx) = async {
        if not (checker ent) then 
          Log.trace(kind, "Entity [%s]: %s", ent.Language, AstOps.formatEntityKind ent.Kind)
          // TODO: DO not create new contexts if it has not changed
          let ngctx = ref None
          let! ntctx = createAnalyzerContext kind checker getAnalyzer setResult input (lazy match ngctx.Value with Some v -> v | _ -> failwith "createAnalyzerContext: Initialization error") langName lang
          ngctx := Some { gctx.Value with Contexts = gctx.Value.Contexts.Add(langName, ntctx) }
          let ctx = gctx.Value.Contexts.[ent.Language]
          let errorCount = ctx.GlobalContext.Errors.Count
          let! res = getAnalyzer(languages.[ent.Language]).Analyze(ent, ctx) 
          ent.Errors <- ent.Errors @ [ for i in errorCount .. ctx.GlobalContext.Errors.Count - 1 -> ctx.GlobalContext.Errors.[i] ]
          Log.trace(kind, "Entity [%s] %s DONE errors: %s, result: %O", ent.Language, AstOps.formatEntityKind ent.Kind, ent.Errors.Length, res)
          setResult ent res } } |> async.Return)

let createCheckingContext = 
  createAnalyzerContext "typechecker"
    (fun ent -> ent.Type.IsSome)
    (fun lang -> defaultArg lang.TypeChecker  defaultTypeChecker)
    (fun ent typ -> ent.Type <- Some typ)

let createInterpreterContext = 
  createAnalyzerContext "interpreter"
    (fun ent -> ent.Value.IsSome)
    (fun lang -> defaultArg lang.Interpreter defaultInterpreter)
    (fun ent (out, res) -> 
        if out <> "" then ent.Console <- Some out
        ent.Value <- Some res)

let startAnalyzer createContext (ent:Entity) = async {
  let gctx = ref None
  let mutable langContexts = Map.empty
  for (KeyValue(name, lang)) in languages do
    let! ctx = createContext (lazy match gctx.Value with Some v -> v | _ -> failwith "startAnalyzer: Initialization error") name lang
    langContexts <- Map.add name ctx langContexts
  gctx := Some (GlobalAnalyzerContext.Create(langContexts))
  do! gctx.Value.Value.Contexts.[ent.Language].Analyze(ent, gctx.Value.Value.Contexts.[ent.Language])
  Log.trace("typechecker", "Errors: %O", gctx.Value.Value.Errors.ToArray()) }

// ------------------------------------------------------------------------------------------------


(*
let blockId = 
  let mutable count = 0 
  fun () -> count <- count + 1; sprintf "block_%d" count

let parseMarkdown tree = 
  let rec loop acc pars = seq {
    match pars with 
    | MarkdownNode "para" [MarkdownNode "inlinecode" [body]]::rest ->
        if not (List.isEmpty acc) then
          let id = blockId()
          yield { Node = { ID = id; BlockKind = { MarkdownBlock.Parsed = List.rev acc }; Errors = [] }; Entity = None; Range = { Block = id; Start = 0; End = 0; } }

        let body = unbox<string> body
        let start = body.IndexOfAny [| '\r'; '\n' |]
        let lang, body = body.[0 .. start].Trim(), body.[start..].Trim() 
        match languages.TryFind(lang) with
        | Some lang ->
            let id = blockId()
            let block, errors = lang.Parse(id, body)
            yield { Node = { ID = id; BlockKind = block; Errors = errors }; Entity = None; Range = { Block = id; Start = 0; End = body.Length } } 
        | _ ->
            failwithf "Unsupported language '%s'" lang

        yield! loop [] rest
    | node::rest ->
        yield! loop (node::acc) rest
    | [] ->
        if not (List.isEmpty acc) then
          let id = blockId()
          yield { Node = { ID = id; BlockKind = { MarkdownBlock.Parsed = List.rev acc }; Errors = [] }; Entity = None; Range = { Block = id; Start = 0; End = 0 } } }

  match tree with 
  | MarkdownNode "markdown" body -> List.ofSeq (loop [] body)
  | _ -> []
*)

// ------------------------------------------------------------------------------------------------

open Wrattler.Html

type ToolsState = 
  | Normal
  | Adding

type State = 
  { BindingContext : BindingContext
    BindingResult : BindingResult
    Nodes : Node<Block> list 
    SelectedNode : string option
    ToolsState : ToolsState
    States : obj list }

type Event = 
  | Refresh
  | StartEvaluation of bool
  | BlockEvent of id:string * obj
  | UpdateNodes of Node<Block> list * obj list
  | UpdateBound of BindingResult
  | SelectNode of string option
  | DeleteBlock of string
  | AddBlock of string * string
  | OpenAddMenu of string option
  | CloseAddMenu 
 
let rec evalEntity bound ent = async {
  do! startAnalyzer (createCheckingContext (makeTcContext bound)) ent
  do! startAnalyzer (createInterpreterContext ()) ent  }

and makeTcContext (bound:BindingResult) = 
  { new TypeCheckingContext with 
      member x.Bound = bound
      member x.Evaluate e = evalEntity bound e }

let typeCheck state id code = async {
  try
    let checkNodes = 
      state.Nodes 
      |> List.skipAfter (fun nd -> nd.Node.ID = id)
      |> List.map (fun nd -> 
          if nd.Node.ID <> id then nd else
          match nd.Node.BlockKind with
          | :? CodeBlock as cb -> 
              let cb, errs = cb.WithCode(code)
              { Range = nd.Range; Entity = None; Node = { ID = id; Errors = errs; BlockKind = cb } } 
          | _ -> failwith "Expected code block")
    let! bound, bindingResult = Binder.bind state.BindingContext checkNodes
    let ctx = makeTcContext bindingResult
    do! startAnalyzer (createCheckingContext ctx) bound
    return bindingResult 
  with e ->
    Log.exn("main", "Type checker failed: %O", e) 
    return raise e }

let startEvaluation trigger state updateAsap evaluate = Async.StartImmediate <| async { 
  try
    let! bound, bindingResult = Binder.bind state.BindingContext state.Nodes
    trigger (UpdateBound(bindingResult))
    if updateAsap then trigger (UpdateNodes(state.Nodes, state.States))
    let ctx = makeTcContext bindingResult
    do! startAnalyzer (createCheckingContext ctx) bound
    if updateAsap then trigger Refresh
    else trigger (UpdateNodes(state.Nodes, state.States))
    if evaluate then
      do! startAnalyzer (createInterpreterContext ()) bound    
      trigger Refresh
  with e ->
    Log.exn("main", "Evaluator failed: %O", e) }

let getColor =   
  let colorMap = System.Collections.Generic.Dictionary<_, _>()
  let mutable index = -1
  let colors = Array.init 6 (sprintf "clr%d")
  fun cat -> 
    if not (colorMap.ContainsKey(cat)) then
      index <- (index + 1) % colors.Length
      colorMap.Add(cat, colors.[index])
    colorMap.[cat]

let renderTools trigger globalState selected id = 
  h?div ["class" => "tools"] <|
  match globalState.ToolsState with
  | Adding when selected ->
      [ for kv in languages do
          if kv.Key <> "system" then
            yield
              h?a 
                [ "href" => "javascript:;"; "title" => "Add " + kv.Key + " cell"
                  "click" =!> fun _ e -> e.cancelBubble <- true; trigger (AddBlock(kv.Key, id)) ] 
                [ h?i ["class" => "fa fa-plus"] []; text kv.Key ]
        yield
          h?a 
            [ "href" => "javascript:;"; "title" => "Cancel"
              "click" =!> fun _ e -> e.cancelBubble <- true; trigger CloseAddMenu ] 
            [ h?i ["class" => "fa fa-times"] []; text "cancel" ]      
      ]
  | _ ->
      ( if id <> "" then
          [ h?a 
              [ "href" => "javascript:;"; "title" => "Delete cell"
                "click" =!> fun _ e -> e.cancelBubble <- true; trigger (DeleteBlock id) ] 
              [ yield h?i ["class" => "fa fa-times"] []
                if selected then yield text "delete" ] ]
        else [] ) @ 
      [ h?a 
          [ "href" => "javascript:;"; "title" => "Add cell"
            "click" =!> fun _ e -> e.cancelBubble <- true; trigger (OpenAddMenu(if id = "" then None else Some id)) ]  
          [ yield h?i ["class" => "fa fa-plus"] []
            if selected then yield text "add" ] ]

let render trigger globalState = 
  h?div [] [
    let selected = None = globalState.SelectedNode
    yield 
      h.elk "div" "block-0" 
        [ "class" => 
              "block block-" + getColor "system" +
              (if selected then "-selected block-selected" else "")
          "click" =!> fun _ _ -> trigger(SelectNode None) ] 
        [ h?div ["class" => "block-body" ] [ 
            h?div ["class" => "block-input"] []
            renderTools trigger globalState selected "" ] ]
    for nd, state in Seq.zip globalState.Nodes globalState.States do
      match languages.[nd.Node.BlockKind.Language].Editor with
      | None -> ()
      | Some ed ->          
          let selected = 
            Some nd.Node.ID = globalState.SelectedNode
          let ctx = 
            { new EditorContext<_> with
                member x.Selected = selected
                member x.Bound = globalState.BindingResult
                member x.TypeCheck(id, code) = typeCheck globalState id code
                member x.Trigger(evt) = trigger(BlockEvent(nd.Node.ID, evt))
                member x.Refresh() = trigger Refresh }
          yield 
            h.elk "div" nd.Node.ID 
              [ "class" => 
                    "block block-" + nd.Node.BlockKind.Language + " block-" + getColor nd.Node.BlockKind.Language +
                    (if selected then "-selected block-selected" else "")
                "click" =!> fun _ _ -> trigger(SelectNode(Some nd.Node.ID)) ] 
              [ h?div ["class" => "block-body" ] [
                  ed.Render(nd.Node.ID, ctx, state)
                  renderTools trigger globalState selected nd.Node.ID
                ]
              ]

    yield 
      h?div ["class" => "block block-" + getColor "system"] [
        h?div ["class" => "block-body" ] [ 
          h?div ["class" => "block-input"] [
            text "Debugging tools: "
            h?a ["click" =!> fun _ _ -> Debugger.visualizeGraph globalState.Nodes 
                 "href" => "javascript:;" ] [text "draw dependency graph"]  ]
          ]
        ]
      ]

let nextBlockId = 
  let mutable counter = 1
  fun () -> 
    counter <- counter + 1
    sprintf "block_%d" counter


let update trigger globalState evt =
  match evt with
  | OpenAddMenu nd ->
      Some { globalState with ToolsState = ToolsState.Adding; SelectedNode = nd }
  | CloseAddMenu ->
      Some { globalState with ToolsState = ToolsState.Normal }

  | AddBlock(lang, afterId) ->
      let id = nextBlockId ()
      let block, errors = languages.[lang].Parse(id, "")
      let newNode = { Node = { ID = id; BlockKind = block; Errors = errors }; Entity = None; Range = { Block = id; Start = 0; End = 0; } }
      let newState = 
        match languages.[lang].Editor with
        | None -> null
        | Some ed -> ed.Initialize(newNode) 
      let newNodes, newStates = 
        if afterId = "" then newNode::globalState.Nodes, newState::globalState.States else 
          List.zip globalState.Nodes globalState.States 
          |> List.collect (fun (nd, st) ->
              if nd.Node.ID = afterId then [nd, st; newNode, newState]
              else [nd, st] )
          |> List.unzip
      trigger CloseAddMenu
      startEvaluation trigger { globalState with Nodes = newNodes; States = newStates } true true
      None

  | DeleteBlock(id) ->
      let newNodes, newStates = 
        List.zip globalState.Nodes globalState.States 
        |> List.filter (fun (nd, st) -> nd.Node.ID <> id)
        |> List.unzip
      startEvaluation trigger { globalState with Nodes = newNodes; States = newStates } true true
      None

  | SelectNode nd ->
      if nd = globalState.SelectedNode then None 
      else Some { globalState with SelectedNode = nd; ToolsState = ToolsState.Normal }

  | Refresh -> 
      Log.trace("gui", "Refresh")
      Some globalState

  | StartEvaluation run ->
      Log.trace("gui", "Start evaluation")
      startEvaluation trigger globalState true run
      None 

  | BlockEvent(id, evt) ->
      Log.trace("gui", "Event in block %s", id)
      let node, state = Seq.zip globalState.Nodes globalState.States |> Seq.find (fun (nd, _) -> nd.Node.ID = id)
      let nstate = 
        match languages.[node.Node.BlockKind.Language].Editor with
        | Some ed -> ed.Update(evt, state)
        | None -> { StartEvaluation = None; Node = node; State = state }
      let newNodes, newStates = 
        List.zip globalState.Nodes globalState.States 
        |> List.map (fun (nd, st) -> 
          if nd.Node.ID = id then nstate.Node, nstate.State else nd, st)
        |> List.unzip
      match nstate.StartEvaluation with
      | Some run -> 
          let newState = { globalState with Nodes = newNodes; States = newStates }          
          startEvaluation trigger newState false run
          None
      | _ -> 
          Some { globalState with Nodes = newNodes; States = newStates }
          
  | UpdateNodes(newNodes, newStates) -> 
      Log.trace("gui", "Update nodes")
      Some { globalState with Nodes = newNodes; States = newStates }

  | UpdateBound(bound) -> 
      Log.trace("gui", "Update binding result")
      Some { globalState with BindingResult = bound }
      

let code lang (src:string) = 
  let id = nextBlockId ()
  let src = src.Split('\n') |> Array.map (fun l -> if l.Length >= 6 then l.Substring(6) else l) 
  let src = String.concat "\n" (if src.[0].Trim() = "" then src.[1..] else src)
  let block, errors = languages.[lang].Parse(id, src)
  { Node = { ID = id; BlockKind = block; Errors = errors }; Entity = None; Range = { Block = id; Start = 0; End = 0; } }

let empty = 
  [ code "markdown" """
      # Wrattler demos
      Click the `+` button on the right to add a new cell. You can choose between different kinds of cells.
      As an exeample, try the following:

      ### Markdown
      You can type any Markdown content in a Markdown cell. Try for example:

      Try: `Hello _world_!`

      ### TheGamma
      You can type TheGamma scripts in TheGamma cell. Try the following and then type `.` to see what is available!

      Try: `web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls")`

      ### R scripts
      You can type R code in R cells. Try the following and hit `Alt+Enter`. You should see newly exposed data sets:

      Try: `test <- iris`

      ### JavaScript
      You can type arbirtrary JavaScript in a JavaScript cell. Try the following and then hit `Alt+Enter` to run the code
      (note that the results are cached, so hitting `Alt+Enter` twice doesn't show the alert again).

      Try: `window.alert("Hello world!")`
      """  ]

let broadband = 
(*
  [ code "markdown" """
      # UK broadband speed
      
      ### Getting the data

      This sample Wrattler notebook looks at the UK broadband speed for the year 2014. 
      We use TheGamma to get the raw data from the UK government web page 
      ([CSV file](https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls)) 
      and group internet speeds by whether they are in urban or rural areas:""" 
    code "gamma" """
      let byUrbanRural =
        web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls")
          .explore.'group data'.'by Urban/rural'.'average Download speed (Mbit/s) 24 hrs'.then
          .'filter data'.'Urban/rural is not'.''.then
          .'get series'.'with key Urban/rural'.'and value Download speed (Mbit/s) 24 hrs'"""
    code "markdown" """
      ### Building Vega visualization
      Now that we obtained the data and performed the aggregation, we use a JavaScript 
      library [Vega Lite](https://vega.github.io/vega-lite/) to display the data as a chart.
      Note that we can run arbitrary JavaScript here - try adding `alert("Hello world!")` and
      hitting `Alt+Enter`.
      """
    code "javascript" """
      var spec = 
        { "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
          "mark": "bar", "width": 800, "height": 300,
          "data": { "values": byUrbanRural },
          "encoding": {
            "x": {"field": "key", "type": "ordinal"},
            "y": {"field": "value", "type": "quantitative"},
            "color": {"field": "key", "type": "nominal", "scale": { "range": ["#90AA4C", "#4B61A8"] }} } }
      addOutput(function(id) { 
        vega.embed("#" + id, spec, {actions:false});
      });"""      
      ]
      *)
  [ code "markdown" """
      # Is broadband in rural areas getting better?
      
      The internet in urban areas in the UK is better than internet in rural areas. In this notebook,
      we analyse the difference between rural and urban areas and we explore whether the situation
      got better between 2014 and 2015. To do this, we use dataset on UK broadband speed
      [published by Ofcom](https://www.ofcom.org.uk/research-and-data/telecoms-research/broadband-research).

      ### Exploring broadband speed
      
      First, we look at the difference in broadband quality between 2014 and 2015. In this early
      exploratory stage, we want to get rapid feedback when writing code and see results immediately.
      In addition, we want to use data transparently and link directly to the government data source.
      The following cell uses TheGamma to explore the data.
      """
    code "gamma" """
      let avgs2014 =
        web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls")
          .explore.'group data'.'by Urban/rural'.'average Download speed (Mbit/s) 24 hrs'.then
          .'filter data'.'Urban/rural is not'.''.then
          .'get series'.'with key Urban/rural'.'and value Download speed (Mbit/s) 24 hrs'

      let avgs2015 =
        web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0015/50073/panellist-data.csv.xls")
          .explore.'group data'.'by URBAN2'.'average DLpeakmean'.then
          .'filter data'.'URBAN2 is not'.'Semi-urban'.then
          .'get series'.'with key URBAN2'.'and value DLpeakmean'

      compost.charts.bar(avgs2014)
      compost.charts.bar(avgs2015)
    """
    code "markdown" """
      ###  Joining broadband data from two years

      Now that we have a basic idea about our data, we want to download the two datasets and clean them
      so that we can do a more rigorous statistical analysis. To do this, we will still use TheGamma scripts.
      First, we get the two files and we manually select columns from 2014 that we want to use for our analysis.
    """
    code "gamma" """
      let bb2014 = 
        web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0014/74120/panellist_data_november_2014.csv.xls")
          .explore.'drop columns'.'drop Id'.'drop Distance band'.'drop Distance band used for weighting'
          .'drop DNS failure (%)24-hour'.'drop DNS failure (%)8-10pm weekday'.'drop DNS resolution (ms)24-hour'
          .'drop DNS resolution (ms)8-10pm weekday'.'drop Download speed (Mbit/s) 8-10pm weekday'.'drop Download speed (Mbit/s) Max'
          .'drop Headline speed'.'drop ISP'.'drop isp weights'.'drop Jitter down (ms)24-hour'.'drop Jitter down (ms)8-10pm weekday'
          .'drop Jitter up (ms)24-hour'.'drop Jitter up (ms)8-10pm weekday'.'drop Latency (ms)8-10pm weekday'.'drop LLU'
          .'drop Market'.'drop nat weights'.'drop Packet loss (%)24-hour'.'drop Packet loss (%)8-10pm weekday'.'drop Technology'
          .'drop Upload speed (Mbit/s)8-10pm weekday'.'drop Upload speed (Mbit/s)Max'.'drop Web page (ms)8-10pm weekday'
          .then.'get the data'
    
      let bb2015 = 
        web.loadTable("https://www.ofcom.org.uk/__data/assets/excel_doc/0015/50073/panellist-data.csv.xls")
          .explore.'get the data'    
    """ 
    code "markdown" """
      As you can see, the 2015 dataset uses different structure than the 2014 dataset. If we want to run any analysis,
      we need to restructure the datasets to use the same format. This is a typical example of tedious task that 
      can be automated by an AI assistant. Wrattler integrates with datadiff, which allows us to do this automatically:
    """
    code "gamma" """
      let bb2015fix = 
        datadiff.adapt(bb2015, bb2014).then.'Delete column WT_national'
          .'Delete all recommended columns'.Result
    """
    code "markdown" "
      ### Analysing broadband change in rural areas

      Next, we would like to perform simpel statistical analysis to asses whether the internet speed in rural
      areas is improving faster than internet speed in urban areas. So far, we did all our work in a simple
      interactive language supported by Wrattler. For statistical analysis, we need to use another language.
      In the following demo, we use R. Note that all data frames defined earlier are automatically available.
      "
    code "r" """
      colnames(bb2014) <- c("Urban","Down","Up","Latency","Web")
      colnames(bb2015fix) <- c("Urban","Down","Up","Latency","Web")

      bball <- rbind(bb2014, bb2015fix)
      bball$Year <- c(rep(2014, nrow(bb2014)), rep(2015, nrow(bb2015fix)))
      bball <- bball %>%
        mutate(IsRural = ifelse(Urban == "Urban", 0, 1),
                YearAfter = ifelse(Year == 2014, 0, 1))
     
      did_model <- lm(Down ~ IsRural + YearAfter + IsRural*YearAfter, data = bball)
      print(summary(did_model))
      """
    (*code "markdown" """
      The previous defines a combined data frame and it prints output to console. Next, we
      will also export a dataframe that can be nicely visualized.
      """
    code "r" """
      colnames(bb2014) <- c("Urban","Down","Up","Latency","Web")
      colnames(bb2015fix) <- c("Urban","Down","Up","Latency","Web")

      training <- bb2014 %>% mutate(Urban = ifelse(Urban=="Urban", 1, 0))
      test <- bb2015fix %>% mutate(Urban = ifelse(Urban=="Urban", 1, 0)) 

      model <- glm(Urban ~.,family=binomial(link='logit'),data=training)
      pred <- predict(model, test, type="response") %>% round
      pred[is.na(pred)] <- 0.5

      combined <- data.frame(Urban=pred, ActualUrban=test$Urban, Ones=rep(1,length(pred)))
      viz <- aggregate(combined$Ones, by=list(combined$ActualUrban, combined$Urban), FUN=sum)
      colnames(viz) <- c("Actual", "Predicted", "Count")
      rm(training,test,pred,combined)
      """*)
    code "markdown" """
      ### Building rich data visualizations

      Finally, we would like to build a data visualization that shows the change of broadband 
      speeds. In this notebook, we build a simple bar chart, but we also want to be able
      to create rich interactive visualizations that let the reader explore further. In 
      Wrattler, we can easily include JavaScript cells that, again, have access to all data
      frames defined earlier. The following sample uses the [Vega lite](https://vega.github.io/vega-lite/) 
      library to build the bar chart.
      """
    code "javascript" """
      var viz = 
        avgs2015.map(function(v){ v.label = v.key + " (2015)"; return v }).concat
          (avgs2014.map(function(v){ v.label = v.key + " (2014)"; return v }));

      var spec = 
        { "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
          "mark": "bar", "width": 800, "height": 300,
          "data": { "values": viz },
          "encoding": {
            "x": {"field": "label", "type": "ordinal"},
            "y": {"field": "value", "type": "quantitative"},
            "color": {"field": "key", "type": "nominal", "scale": { "range": ["#90AA4C", "#4B61A8"] }} } }
      addOutput(function(id) { 
        vega.embed("#" + id, spec, {actions:false});
      });
      """(*
    code "javascript" """
      function lookup(act, pred) {
        for(var i = 0; i < 4; i++) 
          if (viz[i].Actual == act && viz[i].Predicted == pred) return viz[i].Count;
      }

      var red = "#E07945", blue = "#649AE0";
      var json = {
        "nodes": [
           {"name":"Rural (Actual 2015)", "color":red}, {"name":"Urban (Actual 2015)", "color":blue},
           {"name":"Rural (Model using 2014)", "color":red}, {"name":"Urban (Model using 2014)", "color":blue}
        ],
        "links": [
          {"source":0,"target":2,"value":lookup(0, 0),"color":red},
          {"source":1,"target":2,"value":lookup(1, 0),"color":red},
          {"source":0,"target":3,"value":lookup(0, 1),"color":blue},
          {"source":1,"target":3,"value":lookup(1, 1),"color":blue}
        ]
      };

      function render(id) {
        document.getElementById(id).style.height = "500px";
        document.getElementById(id).style.width = "80%";
        document.getElementById(id).style.paddingLeft = "10%";
        d3.select("#" + id).append("svg").chart("Sankey.Path")
          .name(function(node) { return node.name; })
          .colorNodes(function(name, node) { return node.color || "#9f9fa3"; })
          .colorLinks(function(link) { return link.color || "#9f9fa3"; })
          .nodeWidth(25)
          .nodePadding(30)
          .spread(true)
          .iterations(0)
          .draw(json);
      }

      addOutput(render);
      """*)
  ]
//*)

let demo = 
  if Browser.window.location.pathname.Contains("broadband") then broadband else empty

let demo2 = 
  if Browser.window.location.hash.Length > 1 then
    demo |> List.truncate (int(Browser.window.location.hash.Substring(1)))
  else demo

let state =
  let nodes = demo2 //parseMarkdown (Markdown.markdown.parse(demo))
  { BindingResult = BindingResult [||]
    BindingContext = Binder.createContext languages
    Nodes = nodes
    ToolsState = ToolsState.Normal
    SelectedNode = None
    States = 
      [ for nd in nodes ->
          match languages.[nd.Node.BlockKind.Language].Editor with
          | None -> null
          | Some ed -> ed.Initialize(nd) ] }

let app = createVirtualDomApp "demo" state render update
Browser.document.body.onclick <- fun _ -> app.Trigger(SelectNode None); null
app.Trigger(StartEvaluation true)
(*

let updatee trigger state evt = 
  state + evt |> Some

let renderr trigger state = 
  h?div [] [
    for i in 1 .. state ->
      h?h1 ["id" => "d" + string i] [
        text ("Hello " + string i)
      ] |> h.func (fun () ->
        let el = Browser.document.getElementById("d" + string i)
        Browser.console.log(el.dataset.["initialized"])
        el.dataset.["initialized"] <- "true"
      )
    yield h?button ["click" =!> fun _ _ -> trigger 1 ] [ text "Yo" ]
  ]

let app = createVirtualDomApp "demo" 1 renderr updatee 
app.Trigger( 0 )

*)
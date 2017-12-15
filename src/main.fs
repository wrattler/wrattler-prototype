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

// ------------------------------------------------------------------------------------------------

open Wrattler.Languages
open Wrattler.Ast.AstOps

let bindBuiltinCodeBlock ctx (cb:CodeBlock) = async {
  let vars = Map.toList ctx.Frames |> List.map snd
  let codeEnt = bindEntity ctx cb.Language (Code(cb.Language, cb.Code, vars))

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

  let vars = vars |> List.map (fun v -> v, bindEntity ctx cb.Language (DataFrame(v, codeEnt)))
  //let frames = vars |> List.fold (fun frames (v, ent) -> Map.add v ent frames) ctx.Frames
  let blockEnt = bindEntity ctx cb.Language (EntityKind.CodeBlock(cb.Language, codeEnt, List.map snd vars)) 
  return blockEnt, vars }

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

type SimpleCodeBlock(lang, code) = 
  interface BlockKind with
    member x.Language = lang
  interface CodeBlock with
    member x.Code = code
    member x.WithCode(newCode) = SimpleCodeBlock(lang, newCode) :> _, []

let rlang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(ctx, nd) = bindBuiltinCodeBlock ctx (nd :?> CodeBlock)
      member x.Editor = Some(Rendering.createStandardEditor ())
      member x.TypeChecker = None
      member x.Interpreter = Some(builtinInterprter Interpreter.evalR)
      member x.Parse(code) = SimpleCodeBlock("r", code) :> _, [] }

let jslang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(ctx, nd) = bindBuiltinCodeBlock ctx (nd :?> CodeBlock)
      member x.Editor = Some(Rendering.createStandardEditor ())
      member x.Interpreter = Some(builtinInterprter Interpreter.evalJs)
      member x.TypeChecker = None
      member x.Parse(code) = SimpleCodeBlock("javascript", code) :> _, [] }

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
      member x.Bind(ctx, _) = 
        let ck = 
          { new CustomEntityKind with 
              member x.Language = "gamma"
              member x.FormatEntity() = "markdown"
              member x.GetCodeAndAntecedents() = [], "<markdown code='??' />" } // TODO
        let ent = bindEntity ctx "markdown" (EntityKind.CustomEntity(ck))
        async.Return(ent, [])

      member x.Editor = 
        { new Editor<_, _> with
            member x.Initialize(nd) = nd 
            member x.Render(ctx, nd) = 
              match nd.Node.BlockKind with 
              | :? MarkdownBlock as mb ->
                  [ h?div ["class" => "block-markdown"] [
                      yield h?div ["class" => "tools"] [
                        h?a ["href" => "javascript:;"] [ h?i ["class" => "fa fa-code"] []; text "edit source" ] 
                      ]
                      let tree = Markdown.markdown.toHTMLTree(Array.ofList(box "markdown"::mb.Parsed)) |> unbox<obj[]>
                      for i in 1 .. tree.Length-1 do yield Rendering.renderHtmlTree tree.[i] ] ]
              | _ -> failwith "mdlang: Unexpected node"
            member x.Update(state, evt) = state } |> Some
      member x.TypeChecker = None
      member x.Interpreter = None
      member x.Parse(code) = failwith "sys parser" }

let syslang = 
  { new LanguagePlugin<obj, obj, _, _> with
      member x.Bind(_, _) = failwith "sys binder"
      member x.Editor = None
      member x.TypeChecker = None
      member x.Interpreter = None
      member x.Parse(code) = failwith "sys parser" }

let languages : Map<string, LanguagePlugin<obj, obj, obj, obj>> = 
  [ "system", syslang
    "markdown", unbox mdlang
    "r", unbox rlang
    "javascript", unbox jslang
    "gamma", unbox Gamma.Plugin.language ] |> Map.ofSeq
 
let rec createAnalyzerContext kind checker (getAnalyzer:_ -> Analyzer<_, _, _>) setResult input (gctx:Lazy<_>) langName (lang:LanguagePlugin<obj, obj, _, _>) = 
  { new AnalyzerContext<obj> with
        member x.GlobalContext = gctx.Value
        member x.Context = getAnalyzer(lang).CreateContext(input)
        member x.Analyze(ent, ctx) = async {
          if not (checker ent) then 
            Log.trace(kind, "Entity [%s]: %s", ent.Language, AstOps.formatEntityKind ent.Kind)
            // TODO: DO not create new contexts if it has not changed
            let rec ntctx = createAnalyzerContext kind checker getAnalyzer setResult input (lazy ngctx) langName lang
            and ngctx = { gctx.Value with Contexts = gctx.Value.Contexts.Add(langName, ntctx) }
            let ctx = gctx.Value.Contexts.[ent.Language]
            let errorCount = ctx.GlobalContext.Errors.Count
            let! res = getAnalyzer(languages.[ent.Language]).Analyze(ent, ctx) 
            ent.Errors <- ent.Errors @ [ for i in errorCount .. ctx.GlobalContext.Errors.Count - 1 -> ctx.GlobalContext.Errors.[i] ]
            Log.trace(kind, "Entity [%s] %s DONE errors: %s, result: %O", ent.Language, AstOps.formatEntityKind ent.Kind, ent.Errors.Length, res)
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
          yield { Node = { ID = blockId(); BlockKind = { MarkdownBlock.Parsed = List.rev acc }; Errors = [] }; Entity = None; Range = { Start = 0; End = 0; } }

        let body = unbox<string> body
        let start = body.IndexOfAny [| '\r'; '\n' |]
        let lang, body = body.[0 .. start].Trim(), body.[start..].Trim() 
        match languages.TryFind(lang) with
        | Some lang ->
            let block, errors = lang.Parse(body)
            yield { Node = { ID = blockId(); BlockKind = block; Errors = errors }; Entity = None; Range = { Start = 0; End = body.Length } } 
        | _ ->
            failwithf "Unsupported language '%s'" lang

        yield! loop [] rest
    | node::rest ->
        yield! loop (node::acc) rest
    | [] ->
        if not (List.isEmpty acc) then
          yield { Node = { ID = blockId(); BlockKind = { MarkdownBlock.Parsed = List.rev acc }; Errors = [] }; Entity = None; Range = { Start = 0; End = 0 } } }

  match tree with 
  | MarkdownNode "markdown" body -> List.ofSeq (loop [] body)
  | _ -> []


// ------------------------------------------------------------------------------------------------

open Wrattler.Html

type State = 
  { BindingContext : BindingContext
    Nodes : Node<Block> list 
    States : obj list }

type Event = 
  | Refresh
  | StartEvaluation of bool
  | BlockEvent of id:string * obj
  | UpdateNodes of Node<Block> list * obj list
  
let startEvaluation trigger state updateAsap evaluate = Async.StartImmediate <| async { 
  try
    let! bound, bindingResult = Binder.bind state.BindingContext state.Nodes
    if updateAsap then trigger (UpdateNodes(state.Nodes, state.States))
    do! startAnalyzer (createCheckingContext bindingResult) bound
    if updateAsap then trigger Refresh
    else trigger (UpdateNodes(state.Nodes, state.States))
    if evaluate then
      do! startAnalyzer (createInterpreterContext ()) bound    
      trigger Refresh
  with e ->
    Log.exn("main", "Failed: %O", e) }

let render trigger state = 
  h.stable [
    for nd, state in Seq.zip state.Nodes state.States do
      match languages.[nd.Node.BlockKind.Language].Editor with
      | None -> ()
      | Some ed ->          
          let ctx = 
            { new EditorContext<_> with
                member x.Trigger(evt) = trigger(BlockEvent(nd.Node.ID, evt))
                member x.Refresh() = trigger Refresh }
          for idx, node in List.indexed (ed.Render(ctx, state)) do
            yield sprintf "%s-%d" nd.Node.ID idx, node ]

let update trigger globalState evt =
  match evt with
  | Refresh -> 
      Log.trace("gui", "Refresh")
      globalState

  | StartEvaluation run ->
      Log.trace("gui", "Start evaluation")
      startEvaluation trigger globalState true run
      globalState

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
          // Return old nodes - evaluation will trigger update when needed
          globalState
      | _ -> 
          { globalState with Nodes = newNodes; States = newStates }
          
  | UpdateNodes(newNodes, newStates) -> 
      Log.trace("gui", "Update nodes")
      { globalState with Nodes = newNodes; States = newStates }
      
let state =
  let nodes = parseMarkdown (Markdown.markdown.parse(demo))
  { BindingContext = Binder.createContext languages (startAnalyzer (createInterpreterContext ()))
    Nodes = nodes
    States = 
      [ for nd in nodes ->
          match languages.[nd.Node.BlockKind.Language].Editor with
          | None -> null
          | Some ed -> ed.Initialize nd ] }

let app = createVirtualDomApp "demo" state render update
app.Trigger(StartEvaluation true)


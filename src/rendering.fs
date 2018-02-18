module Wrattler.Rendering

open Wrattler.Ast
open Wrattler.Common
open Wrattler.Html
open Wrattler.Languages
open Fable.Import
open Fable.Import.Monaco
open Fable.Core

// ------------------------------------------------------------------------------------------------

let rec formatHtmlTree tree : string =
  if isString tree then unbox tree
  elif isArray tree then
    let arr = unbox<obj[]> tree
    let contentIdx, props = 
      if isObject arr.[1] then 
        let props = JsHelpers.properties(arr.[1])
        2, String.concat " " [for p in props -> sprintf "%s=%s" p.key (unbox p.value) ]
      else 1, ""
    sprintf "<%s%s>%s</%s>" (unbox arr.[0]) props (String.concat "" [ for i in contentIdx .. arr.Length-1 -> formatHtmlTree arr.[i] ]) (unbox arr.[0])
  else failwithf "Unexpected node: %A" tree

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

let renderObjectsTable objs = 
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
        for obj in Seq.truncate 20 objs -> 
          h?tr [] [
            for prop in props -> h?td [] [ text(string (getProperty obj prop.key))  ]
          ]
      ]
    ]
  ]

let renderTable url trigger = 
  match Datastore.tryFetchPreview url trigger with 
  | None -> h?div ["class" => "preview"] [ h?p [] [text "Loading..."] ]
  | Some objs -> renderObjectsTable objs

let mutable counter = 1
let renderFutureTable (f:Future<_>) trigger = 
  match f.Value with 
  | Some table ->
      renderObjectsTable (Array.map snd table)
  | None -> 
      counter <- counter + 1
      h?div ["class" => "preview"] [ h?p [] [text "Loading..."] ]
      |> h.once (sprintf "future-table-%d" counter) (fun el -> 
        f.Then(fun table -> renderObjectsTable (Array.map snd table) |> Html.renderTo el))

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

// ------------------------------------------------------------------------------------------------

type StandardEditorState = 
  { Node : Node<Block>
    Block : string
    Position : int
    SelectedVariable : string option }

type StandardEditorEvent = 
  | DisplayVariable of string
  | UpdateCode of bool * string
  | UpdatePosition of int

let renderStandardEditor elid renderEntity onCreated (ctx:EditorContext<_>) state lang src =
  h?div [] [ 
    h?div [] [] |> h.once ("block-editor-" + elid) (fun el ->
        h?div ["class" => "block-input"] [
          h?div ["id" => elid + "_editor" ] []
        ] |> renderTo el

        let ed = createMonacoEditor (elid + "_editor") lang src 
        onCreated (ctx, state.Node.Node.ID, ed)

        let mutable conv = LocationMapper(src)
        let updatePosition () =
          let position = ed.getPosition()
          let loc = conv.LineColToAbsolute(int position.lineNumber, int position.column)
          ctx.Trigger(UpdatePosition(loc)) 

        ed.onDidBlurEditor(fun ce -> ctx.Trigger(UpdatePosition(-1)) ) |> ignore
        ed.onDidFocusEditor(fun _ -> updatePosition ()) |> ignore
        ed.onDidChangeCursorPosition(fun _ -> updatePosition ()) |> ignore

        ed.onDidChangeModelContent(fun me ->
          let source = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
          conv <- LocationMapper(source)
          ctx.Trigger(UpdateCode(false, source))  ) |> ignore
        ed.onKeyDown(fun ke -> 
          if ke.altKey && ke.keyCode = KeyCode.Enter then
            ctx.Trigger(UpdateCode(true, ed.getModel().getValue(editor.EndOfLinePreference.LF, false)))  ) |> ignore )

    h?div [] [
      match state.Node.Entity with
      | Some ent ->
          yield h?ul[] [ for e in ent.Errors -> h?li [] [ text e.Message ] ]
      | _ -> () 
    ]

    h?div ["class" => "block-output"] [
      match state.Node.Entity with 
      | Some ent -> yield! renderEntity ctx state ent
      | None -> yield h?p [] [ text "No entity" ] 
    ]
  ]

let createStandardEditor renderEntity onCreated =
  { new Editor<StandardEditorEvent, StandardEditorState> with 
      member x.Initialize(node) = { Block = node.Node.ID; Node = node; SelectedVariable = None; Position = -1 }
      member x.Render(id, ctx, state) = 
        match state.Node.Node.BlockKind with
        | :? CodeBlock as cb -> renderStandardEditor id renderEntity onCreated ctx state cb.Language cb.Code
        | _ -> failwith "createStandardEditor: Wrong block kind"
      member x.Update(evt, state) = 
        match evt with 
        | UpdatePosition(n) ->
            { StartEvaluation = None; Node = state.Node; State = { state with Position = n } }
        | DisplayVariable n -> 
            { StartEvaluation = None; Node = state.Node; State = { state with SelectedVariable = Some n } }
        | UpdateCode(run, src) -> 
            let cb, errs = 
              match state.Node.Node.BlockKind with
              | :? CodeBlock as cb -> cb.WithCode(src)
              | _ -> failwith "createStandardEditor: Wrong block kind"
            let nd = { state.Node.Node with BlockKind = cb; Errors = errs }
            let st = { state with Node = { Range = state.Node.Range; Node = nd; Entity = None } } 
            { StartEvaluation = Some run; Node = st.Node; State = st } }

// ------------------------------------------------------------------------------------------------

type ToggleEditorState = 
  { Node : Node<Block> 
    Source : string 
    Language : string }

type ToggleEditorEvent = 
  | UpdateCode of string

let renderToggleEditor elid renderView onCreated (ctx:EditorContext<_>) state lang src =
  if ctx.Selected then 
    h.elk "div" (elid + "-editor") [] [] |> h.once ("block-editor-" + elid) (fun el ->
        h?div ["class" => "block-input"] [
          h?div ["id" => elid + "_editor" ] []
        ] |> renderTo el

        let ed = createMonacoEditor (elid + "_editor") lang src 
        onCreated (ctx, state.Node.Node.ID, ed)

        ed.onDidChangeModelContent(fun me ->
          let source = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
          ctx.Trigger(UpdateCode(source))  ) |> ignore )
  else 
    h.elk "div" (elid + "-view") [] [
      renderView state
    ]

let createToggleEditor renderView onCreated =
  { new Editor<ToggleEditorEvent, ToggleEditorState> with 
      member x.Initialize(node) =
        match node.Node.BlockKind with
        | :? CodeBlock as cb -> { Node = node; Source = cb.Code; Language = cb.Language }
        | _ -> failwith "createToggleEditor: Wrong block kind"

      member x.Render(id, ctx, state) = 
        match state.Node.Node.BlockKind with
        | :? CodeBlock as cb -> renderToggleEditor id renderView onCreated ctx state cb.Language cb.Code
        | _ -> failwith "createToggleEditor: Wrong block kind"
      member x.Update(evt, state) =
        match evt with
        | UpdateCode newCode ->
            let cb, errs = 
              match state.Node.Node.BlockKind with
              | :? CodeBlock as cb -> cb.WithCode(newCode)
              | _ -> failwith "createToggleEditor: Wrong block kind"
            let nd = { state.Node.Node with BlockKind = cb; Errors = errs }
            let st = { state with Node = { Range = state.Node.Range; Node = nd; Entity = None } } 
            { StartEvaluation = None; Node = st.Node; State = st } }

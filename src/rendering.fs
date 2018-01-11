module Wrattler.Rendering

open Wrattler.Ast
open Wrattler.Common
open Wrattler.Html
open Wrattler.Languages
open Fable.Import
open Fable.Import.Monaco
open Fable.Core

// ------------------------------------------------------------------------------------------------

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
      h?div ["class" => "preview"] [ h?pp [] [text "Loading..."] ]
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

type CodeEditorState = 
  { Node : Node<Block>
    SelectedVariable : string option }

type CodeEditorEvent = 
  | DisplayVariable of string
  | UpdateCode of bool * string

let renderEditor onCreated (ctx:EditorContext<_>) state lang src =
  [ h.custom 
      (fun elid ->
        h?div ["class" => "block-input"] [
          h?div ["class" => "tools"] [
            h?a ["href" => "javascript:;"] [ h?i ["class" => "fa fa-code"] []; text "hide source" ] 
          ]
          h?div ["id" => elid + "_editor" ] []
        ] |> renderTo (Browser.document.getElementById(elid))

        let ed = createMonacoEditor (elid + "_editor") lang src 
        onCreated (ctx, state.Node.Node.ID, ed)
        ed.onDidChangeModelContent(fun me ->
          ctx.Trigger(UpdateCode(false, ed.getModel().getValue(editor.EndOfLinePreference.LF, false)))  ) |> ignore
        ed.onKeyDown(fun ke -> 
          if ke.altKey && ke.keyCode = KeyCode.Enter then
            ctx.Trigger(UpdateCode(true, ed.getModel().getValue(editor.EndOfLinePreference.LF, false)))  ) |> ignore
        ed )
      (fun ed -> () )

    h?div [] [
      match state.Node.Entity with
      | Some ent ->
          yield h?ul[] [ for e in ent.Errors -> h?li [] [ text e.Message ] ]
      | _ -> () 
    ]

    h?div ["class" => "block-output"] [
      match state.Node.Entity with
      | Some { Kind = EntityKind.CodeBlock("javascript", { Kind = EntityKind.Code(_, code, _); Value = Some(Outputs outs) }, _) } ->
          // TODO: Use entity symbol for h.delayed
          for out, i in Seq.zip outs [0 .. outs.Length-1] do
            let id = sprintf "output_%d_%d" i (hash code)
            yield h.delayed id (text "") (fun id -> out id)

      | Some { Kind = EntityKind.CodeBlock(_, _, vars) } ->
          let vars = vars |> List.choose (function { Kind = DataFrame(var, _); Value = Some(Frame value) } -> Some(var, value) | _ -> None)
          if not (List.isEmpty vars) then
            let selected = defaultArg state.SelectedVariable (fst(List.head vars))
            yield h?ul ["class" => "nav nav-pills"] [
              for v, data in vars do 
              yield h?li ["class" => "nav-item"] [
                h?a [
                  "class" => (if v = selected then "nav-link active" else "nav-link")
                  "click" =!> fun _ _ -> ctx.Trigger(DisplayVariable(v))
                  "href" => "#" ] [text v]
              ]
            ]
            for v, data in vars do
              if v = selected then 
                yield renderTable data ctx.Refresh
      | ent ->
          yield h?ppp [] [ text (sprintf "Entity: %A" ent) ]
    ]
  ]

let createStandardEditor onCreated =
  { new Editor<CodeEditorEvent, CodeEditorState> with 
      member x.Initialize(node) = { Node = node; SelectedVariable = None }
      member x.Render(ctx, state) = 
        match state.Node.Node.BlockKind with
        | :? CodeBlock as cb -> renderEditor onCreated ctx state cb.Language cb.Code
        | _ -> failwith "createStandardEditor: Wrong block kind"
      member x.Update(evt, state) = 
        match evt with 
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



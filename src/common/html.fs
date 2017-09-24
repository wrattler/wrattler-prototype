module Wrattler.Html

open Fable.Core
open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
open Wrattler.Common

module FsOption = FSharp.Core.Option

[<Emit("$0[$1]")>]
let private getProperty (o:obj) (s:string) = failwith "!"

[<Emit("$0[$1] = $2")>]
let private setProperty (o:obj) (s:string) (v:obj) = failwith "!"

[<Fable.Core.Emit("event")>]
let private event () : Event = failwith "JS"

type DomAttribute = 
  | Event of (HTMLElement -> Event -> unit)
  | Attribute of string
  | Property of obj

type DomNode = 
  | Text of string
  | Delayed of string * DomNode * (string -> unit)
  | Stable of (string * DomNode) list
  | Custom of (string -> obj) * (obj -> unit)
  | Element of ns:string * tag:string * attributes:(string * DomAttribute)[] * children : DomNode[] * onRender : (HTMLElement -> unit) option

let createTree ns tag args children =
  let attrs = ResizeArray<_>()
  let props = ResizeArray<_>()
  for k, v in args do
    match k, v with 
    | k, Attribute v ->
        attrs.Add (k, box v)
    | k, Property o ->
        props.Add(k, o)
    | k, Event f ->
        props.Add ("on" + k, box (fun o -> f (getProperty o "target") (event()) ))
  let attrs = JsInterop.createObj attrs
  let ns = if ns = null || ns = "" then [] else ["namespace", box ns]
  let props = JsInterop.createObj (Seq.append (ns @ ["attributes", attrs]) props)
  let elem = Virtualdom.h(tag, props, children)
  elem

let mutable counter = 0

let rec renderVirtual node = 
  match node with
  | Text(s) -> 
      box s
  | Element(ns, tag, attrs, children, None) ->
      createTree ns tag attrs (Array.map renderVirtual children)
  | Delayed(symbol, body, func) ->
      counter <- counter + 1
      let id = sprintf "delayed_%d" counter

      // Virtual dom calls our hook when it creates HTML element, but
      // we still need to wait until it is added to the HTML tree
      let rec waitForAdded n (el:HTMLElement) = 
        if el.parentElement <> null then 
          el?dataset?renderedSymbol <- symbol
          el?id <- id
          func id
        elif n > 0 then window.setTimeout((fun () -> waitForAdded  (n-1) el), 1) |> ignore
        else Log.error("html", "Delayed element was not created in time")

      // Magic as per https://github.com/Matt-Esch/virtual-dom/blob/master/docs/hooks.md
      let Hook = box(fun () -> ())
      Hook?prototype?hook <- fun (node:HTMLElement) propertyName previousValue ->
        if unbox node?dataset?renderedSymbol <> symbol then
          waitForAdded 10 node
      let h = createNew Hook ()

      createTree null "div" ["renderhk", Property h] [| renderVirtual body |]
  | Element _ ->
      failwith "renderVirtual: Does not support elements with after-render handlers"
  | Custom _
  | Stable _ ->
      failwith "renderVirtual: Unexpected stable or custom inside DOM node"

let rec render node = 
  match node with
  | Custom _
  | Stable _ ->
      failwith "Stable not supported by render"

  | Text(s) -> 
      document.createTextNode(s) :> Node, ignore

  | Delayed(_, _, func) ->
      counter <- counter + 1
      let el = document.createElement("div")
      el.id <- sprintf "delayed_%d" counter
      el :> Node, (fun () -> func el.id)

  | Element(ns, tag, attrs, children, f) ->
      let el = 
        if ns = null || ns = "" then document.createElement(tag)
        else document.createElementNS(ns, tag) :?> HTMLElement
      let rc = Array.map render children
      for c, _ in rc do el.appendChild(c) |> ignore
      for k, a in attrs do 
        match a with
        | Property(o) -> setProperty el k o
        | Attribute(v) -> el.setAttribute(k, v)
        | Event(f) -> el.addEventListener(k, U2.Case1(EventListener(f el)))
      let onRender () = 
        for _, f in rc do f()
        f |> FsOption.iter (fun f -> f el)
      el :> Node, onRender

let renderTo (node:HTMLElement) dom = 
  while box node.lastChild <> null do ignore(node.removeChild(node.lastChild))
  let el, f = render dom
  node.appendChild(el) |> ignore
  f()

type VirtualDomApp<'TEvent> = 
  { Trigger : 'TEvent -> unit }

type Patch<'TState, 'TCreator> = 
  | New of 'TCreator
  | Update of 'TState * 'TCreator
  | Delete of 'TState

let patchLists oldList newList = 
  let newListLookup = dict newList
  let toRemove = set [ for k, _ in oldList do if not (newListLookup.ContainsKey k) then yield k ]
  let rec loop acc oldList newList = 
    match oldList, newList with
    | (ko, vo)::oldList, newList when toRemove.Contains ko -> loop ((ko, Delete vo)::acc) oldList newList
    | (ko, vo)::oldList, (kn, vn)::newList when ko = kn -> loop ((ko, Update(vo, vn))::acc) oldList newList
    | oldList, (kn, vn)::newList -> loop ((kn, New vn)::acc) oldList newList
    | (ko, vo)::oldList, [] -> loop ((ko, Delete vo)::acc) oldList []
    | [], [] -> List.rev acc
  loop [] oldList newList

let createVirtualDomApp id initial r u = 
  let event = new Event<'T>()
  let trigger e = event.Trigger(e)  
  let mutable blocks = []
  let mutable state = initial
  let container = document.getElementById(id)
  container.innerHTML <- ""

  let handleEvent evt = 
    state <- match evt with Some e -> u trigger state e | _ -> state
    let newTree = r trigger state 
    let newBlocks = match newTree with Stable list -> list | dom -> ["it", dom]
    let mutable lastBlock = None
    blocks <- patchLists blocks newBlocks |> List.choose (fun (k, patch) ->
      match patch with 
      | Delete _ ->  
          container.removeChild(document.getElementById(id + "_" + k)) |> ignore
          None
      | New dom ->
          let node = document.createElement("div") :> Node
          (node :?> HTMLDivElement).id <- id + "_" + k
          match lastBlock with 
          | None -> container.insertBefore(node, container.firstChild) |> ignore
          | Some (last:Node) -> container.insertBefore(node, last.nextSibling) |> ignore
          lastBlock <- Some node
          
          match dom with 
          | Custom (create, update) -> 
              let o = create (id + "_" + k)
              Some(k, (node, o))
          | dom ->
              let tree = Fable.Core.JsInterop.createObj []
              let newTree = renderVirtual dom
              let patches = Virtualdom.diff tree newTree
              Some(k, (Virtualdom.patch node patches, newTree))

      | Update((node, o), Custom(_, update)) ->
          lastBlock <- Some node
          update o
          Some(k, (node, o))

      | Update((node, tree), dom) ->
          lastBlock <- Some node
          let newTree = renderVirtual dom
          let patches = Virtualdom.diff tree newTree
          Some(k, (Virtualdom.patch node patches, newTree)) )
  
  handleEvent None
  event.Publish.Add(Some >> handleEvent)
  { Trigger = trigger }
  
let text s = Text(s)
let (=>) k v = k, Attribute(v)
let (=!>) k f = k, Event(f)


type El(ns) = 
  member x.Namespace = ns
  static member (?) (el:El, n:string) = fun a b ->
    Element(el.Namespace, n, Array.ofList a, Array.ofList b, None)

  member x.el(n:string) = fun a b ->
    Element(x.Namespace, n, Array.ofList a, Array.ofList b, None)

  member x.delayed sym body f =
    Delayed(sym, body, f)

  member x.stable list = 
    Stable(list)    

  member x.custom (create:string -> 'T) (update:'T -> unit) = 
    Custom((fun s -> box (create s)), (unbox >> update))    

let h = El(null)
let s = El("http://www.w3.org/2000/svg")

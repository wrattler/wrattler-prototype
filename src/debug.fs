module Wrattler.Debugger
#nowarn "40"

open Fable.Core
open Fable.Import.Browser
open Wrattler.Ast

let vegaColors = 
  ( "#1f77b4,#aec7e8,#ff7f0e,#ffbb78,#2ca02c,#98df8a,#d62728,#ff9896,#9467bd,#c5b0d5," + 
    "#8c564b,#c49c94,#e377c2,#f7b6d2,#7f7f7f,#c7c7c7,#bcbd22,#dbdb8d,#17becf,#9edae5" ).Split(',')

type Node = {id:string; label:string; color:string; (*level:int*) }
type Edge = {from:string; ``to``:string; arrows:string}
type Data = {nodes:obj; edges:obj}
type HierarchicalLayout = {direction:string; sortMethod:string}
type Layout = { improvedLayout:bool; hierarchical:HierarchicalLayout }
type Options = { width:string; height:string; layout:Layout }

[<Emit("new vis.DataSet($0)")>]
let newDataSet (o:obj) : obj = failwith "JS"
[<Emit("new vis.Network($0, $1, $2)")>]
let newNetwork (el:HTMLElement) (data:Data) (options:Options) = failwith "JS"

let visualizeGraph (blocks:Node<Block> list) =  

  let nodes = ResizeArray<_>()
  let edges = ResizeArray<_>()
  let visited = System.Collections.Generic.Dictionary<_, _>()
  let colorMap = System.Collections.Generic.Dictionary<_, _>()
  let getColor key = 
    if not (colorMap.ContainsKey(key)) then colorMap.[key] <- vegaColors.[colorMap.Count % vegaColors.Length]
    colorMap.[key]

  let rec walkGraph level parentId (ent:Entity) = 
    let name = AstOps.formatEntityKind ent.Kind
    edges.Add { from = parentId; ``to`` = ent.Symbol.ToString(); arrows = "to" }
    if not (visited.ContainsKey (ent.Symbol.ToString())) then
      visited.[ent.Symbol.ToString()] <- true
      nodes.Add { (*level = level;*) id = ent.Symbol.ToString(); label = name; color = getColor ent.Language }
      let deps, _ = AstOps.entityCodeAndAntecedents ent.Kind
      for dep in deps do walkGraph (level + 1) (ent.Symbol.ToString()) dep

  for i, nd in Seq.indexed blocks do
    let id = sprintf "block_%d" i
    nodes.Add { id = id; label = sprintf "Block #%d" i; color = getColor "block"; (*level = 0*) }
    match nd.Entity with 
    | Some ent -> walkGraph 1 id ent
    | _ -> ()

  let nodes = newDataSet (nodes.ToArray())
  let edges = newDataSet (edges.ToArray())
  let data = { nodes = nodes; edges = edges }
  let options = { width = "100%"; height = "100%"; layout = { improvedLayout = false; hierarchical = { sortMethod="directed"; direction = "LR" } } }
  let wg = document.getElementById("wrattlergraph")
  wg.style.display <- "block";
  let wgo = document.getElementById("wrattlergraph-output")
  newNetwork wgo data options



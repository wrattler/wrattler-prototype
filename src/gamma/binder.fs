﻿// ------------------------------------------------------------------------------------------------
// Binder attaches `Entity` objects to `Node` objects produced by the parser
// Entities are reused when possible and contain inferred types etc.
// ------------------------------------------------------------------------------------------------
module Wrattler.Gamma.Binder

open Wrattler
open Wrattler.Ast
open Wrattler.Ast.AstOps
open Wrattler.Binder
open Wrattler.Common
open Wrattler.Gamma.Ast
open Wrattler.Gamma.AstOps
open Wrattler.Languages

/// As we bind, we keep root entity, current scope & variables in scope
type GammaBindingContext = 
  { Variables : Map<Name, Entity>  
    GlobalValues : Map<Name, Entity>

    /// When we are in `foo(fun x -> ...)` the `x` is linked to the call site 
    CallSite : Entity option
    /// When we are in `foo.[name:x].bar` the Chain represents `foo` so that `x` can be a member
    Chain : Entity option
    
    Parent : BindingContext
    }
(*
/// Represents result of binding syntax tree to entities 
/// (provides access to all bound entities & children lookup function)
type BindingResult(ents:(Range * Entity)[]) = 
  let childrenLookup = 
    let res = System.Collections.Generic.Dictionary<Symbol, ResizeArray<Entity>>()
    let add a e = 
      if not (res.ContainsKey(a)) then res.Add(a, ResizeArray())
      res.[a].Add(e)
    for _, e in ents do
      for a in e.Antecedents do
        add a.Symbol e
    res 
  member x.Entities = ents
  member x.GetChildren(ent:Entity) = 
    match childrenLookup.TryGetValue(ent.Symbol) with true, res -> res.ToArray() | _ -> [||]

/// Lookup entity (if it can be reused) or create & cache a new one
let bindEntity ctx kind =
  let code, antecedents, name = entityCodeNameAndAntecedents kind
  let symbols = ctx.Root::antecedents |> List.map (fun a -> a.Symbol)
  let nestedDict = 
    match ListDictionary.tryFind symbols ctx.Table with
    | None -> Map.empty
    | Some res -> res
  if nestedDict.ContainsKey (code, name) then 
    Log.trace("binder", "Cached: binding %s %s", formatGammaEntityKind kind, name)
    nestedDict.[code, name]
  else
    Log.trace("binder", "New: binding %s %s", formatGammaEntityKind kind, name)
    let symbol = createSymbol ()
    let entity = { Kind = kind; Symbol = symbol; Type = None; Errors = []; Meta = []; Value = None }
    ListDictionary.set symbols (Map.add (code, name) entity nestedDict) ctx.Table
    entity    

/// Assign entity to a node in parse tree
let setEntity ctx node entity = 
  ctx.Bound.Add(node.Range, entity)
  node.Entity <- Some entity
  entity
*)

let bindParentEntity = bindEntity

let bindEntity ctx ent = 
  bindEntity ctx.Parent "gamma" (EntityKind.CustomEntity(GammaEntityWrapper(ent)))

let setEntity ctx node ent = 
  setEntity ctx.Parent node ent

/// Bind entities to expressions in the parse tree
/// (See `GammaEntityKind` for explanation of how the entity tree looks like)
let rec bindExpression ctx (node:Node<_>) = 
  let bindCallArgExpression site ctx = bindExpression { ctx with CallSite = Some site; Chain = None }
  let bindMemberExpression chain ctx = bindExpression { ctx with CallSite = None; Chain = Some chain }
  let bindPlaceExpression ctx = bindExpression { ctx with CallSite = None }
  let bindExpression ctx = bindExpression { ctx with CallSite = None; Chain = None }

  match node.Node with
  | Expr.Placeholder(name, body) ->
      // Keep `ctx.Chain` in case the plceholder contains member access
      let bodyEnt = bindPlaceExpression ctx body
      bindEntity ctx (GammaEntityKind.Placeholder(name.Node, bodyEnt)) |> setEntity ctx node |> ignore
      bodyEnt

  | Expr.Variable(name) ->
      // Variable is actually member access inside chain or placeholder inside chain
      match ctx.Chain with
      | Some chain -> 
          let memberName = bindEntity ctx (GammaEntityKind.MemberName(name.Node)) |> setEntity ctx name
          bindEntity ctx (GammaEntityKind.Member(chain, memberName)) |> setEntity ctx node 
      | _ -> 
      // Variable is a local variable defined somewhere in context
      match ctx.Variables.TryFind name.Node with 
      | Some decl -> bindEntity ctx (GammaEntityKind.Variable(name.Node, decl)) |> setEntity ctx node
      | _ ->
      // Variable is a global, known or unknown variable
      match ctx.GlobalValues.TryFind name.Node with 
      | Some glob -> glob |> setEntity ctx node
      | _ -> bindEntity ctx (GammaEntityKind.GlobalValue(name.Node, None)) |> setEntity ctx node

  | Expr.Call(instExpr, argsNode) ->
      // Bind instance & create call site that depends on it
      let inst = bindExpression ctx instExpr
      let site arg = bindEntity ctx (GammaEntityKind.CallSite(inst, arg))
      // Bind arguments - which depend on the call site
      let args = argsNode.Node |> List.mapi (fun idx arg -> 
          let site = site (match arg.Name with Some n -> Choice1Of2 n.Node | _ -> Choice2Of2 idx)
          let expr = bindCallArgExpression site ctx arg.Value
          match arg.Name with 
          | Some n -> bindEntity ctx (GammaEntityKind.NamedParam(n.Node, expr)) |> setEntity ctx n
          | None -> expr)
      let args = bindEntity ctx (GammaEntityKind.ArgumentList(args)) |> setEntity ctx argsNode
      bindEntity ctx (GammaEntityKind.Call(inst, args)) |> setEntity ctx node 

  | Expr.Member(instExpr, memExpr) ->
      let instEnt = bindExpression ctx instExpr
      let memEnt = bindMemberExpression instEnt ctx memExpr 
      bindEntity ctx (GammaEntityKind.MemberAccess(memEnt)) |> setEntity ctx node

  | Expr.Binary(lExpr, op, rExpr) ->
      let lEnt = bindExpression ctx lExpr
      let rEnt = bindExpression ctx rExpr
      bindEntity ctx (GammaEntityKind.Operator(lEnt, op.Node, rEnt)) |> setEntity ctx node

  | Expr.List(elExprs) ->
      let elEnts = elExprs |> List.map (bindExpression ctx)
      bindEntity ctx (GammaEntityKind.List(elEnts)) |> setEntity ctx node

  | Expr.Function(var, bodyExpr) ->
      let callSite = match ctx.CallSite with Some s -> s | None -> failwith "bindExpression: Function missing call site"
      let varEnt = bindEntity ctx (GammaEntityKind.Binding(var.Node, callSite)) |> setEntity ctx var
      let bodyEnt = bindExpression { ctx with Variables = Map.add var.Node varEnt ctx.Variables } bodyExpr
      bindEntity ctx (GammaEntityKind.Function(varEnt, bodyEnt)) |> setEntity ctx node

  | Expr.Boolean b -> bindEntity ctx (GammaEntityKind.Constant(Constant.Boolean b)) |> setEntity ctx node
  | Expr.String s -> bindEntity ctx (GammaEntityKind.Constant(Constant.String s)) |> setEntity ctx node
  | Expr.Number n -> bindEntity ctx (GammaEntityKind.Constant(Constant.Number n)) |> setEntity ctx node
  | Expr.Empty -> bindEntity ctx (GammaEntityKind.Constant Constant.Empty) |> setEntity ctx node

/// Bind entities to a command in a parse tree. The handling of `let` is similar
/// to the handling of lambda abstraction. This adds variables to context - we ignore
/// bound entities, because nothing depends on it (except via variables)
let bindCommand ctx (node:Node<_>) =
  match node.Node with
  | Command.Let(v, e) ->
      let body = bindExpression ctx e 
      let var = bindEntity ctx (GammaEntityKind.Variable(v.Node, body)) |> setEntity ctx v
      let frame = bindParentEntity ctx.Parent "gamma" (EntityKind.DataFrame(v.Node, body))
      let node = bindEntity ctx (GammaEntityKind.LetCommand(var, frame, body)) |> setEntity ctx node
      { ctx with Variables = Map.add v.Node var ctx.Variables }, node

  | Command.Expr(e) ->
      let body = bindExpression ctx e 
      let node = bindEntity ctx (GammaEntityKind.RunCommand(body)) |> setEntity ctx node
      ctx, node

/// Bind entities to all nodes in the program
let bindProgram ctx (program:Program) =
  let _, ents = 
    program.Body.Node |> List.fold (fun (ctx, nodes) cmd -> 
      let ctx, node = bindCommand ctx cmd
      ctx, node::nodes) (ctx, [])  
  bindEntity ctx (GammaEntityKind.Program(List.rev ents)) |> setEntity ctx program.Body
  
/// Create a new binding context - this stores cached entities
let createContext ctx (globals:list<string * Entity>) =
  { Parent = ctx 
    Variables = Map.empty 
    GlobalValues = Map.ofList globals
    CallSite = None; Chain = None }

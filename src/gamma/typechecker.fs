// ------------------------------------------------------------------------------------------------
// Type checker sets the Type properties of the Entities created by the Binder
// ------------------------------------------------------------------------------------------------
module Wrattler.Gamma.TypeChecker

open Wrattler
open Wrattler.Ast
open Wrattler.Ast.AstOps
open Wrattler.Gamma
open Wrattler.Gamma.Binder
open Wrattler.Gamma.Ast
open Wrattler.Gamma.AstOps
open Wrattler.Common
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Type checking 
// ------------------------------------------------------------------------------------------------

type CheckingContext = 
  { Globals : IDictionary<string, Entity> 
    Ranges : IDictionary<Symbol, Range>
    Evaluate : Entity -> Value option }

let addError (ctx:Languages.AnalyzerContext<_>) (ent:Entity) err = 
  ctx.GlobalContext.Errors.Add(err ctx.Context.Ranges.[ent.Symbol])

/// Given a list of types, find the most frequent type (using Type.Any as the last resort)
let inferListType typs = 
  typs 
  |> List.filter (function Type.Any -> false | _ -> true)
  |> List.groupWith AstOps.typesEqual
  |> List.map (fun g -> List.head g, List.length g)
  |> List.append [Type.Any, 0]
  |> List.maxBy snd
  |> fst

/// Resolve type of parameter - parSpec can be Choice1Of2 with 
/// parameter name or Choice2Of2 with parameter index.
let resolveParameterType instTy parSpec = 
  match instTy with
  | Type.Method(args, _) -> 
      let par = 
        match parSpec with
        | Choice1Of2 name -> args |> Seq.tryFind (fun ma -> ma.Name = name)
        | Choice2Of2 idx -> args |> Seq.tryItem idx  
      match par with
      | Some ma -> ma.Type
      | _ -> failwith "resolveParameterType: Parameter specification was incorrect"
  | _ -> failwith "resolveParameterType: Instance is not an object"

/// Get type of an entity and record errors generated when type checking this entity
let getType (e:Entity) = 
  if e.Type.IsNone then failwith "getType: Type was missing"
  e.Type.Value 

/// Get type of an entity and record errors generated when type checking this entity
let getGammaType (e:Entity) = 
  if e.Type.IsNone then failwith "getType: Type was missing"
  e.Type.Value :?> Type

/// Check method call - methodName is for logging only; parameterTypes and resultTypeFunc
/// are the type information from `Type.Method` of the parent; `argList` and `args` are the
/// actual type-checked arguments (argList is for storing errors only)
let checkMethodCallAsync (methodName:string) (ctx:Languages.AnalyzerContext<_>) (parameterTypes:MethodArgument list) 
    (resultTypeFunc:((Type * Value option) list -> Type option)) argList args = async {

  // Split arguments into position & name based and report 
  // error if there is non-named argument after named argument
  let positionBased, nameBased = 
    let pb = args |> List.takeWhile (function { Kind = GammaEntity(GammaEntityKind.NamedParam _) } -> false | _ -> true)  
    let nb = args |> List.skipWhile (function { Kind = GammaEntity(GammaEntityKind.NamedParam _) } -> false | _ -> true)  
    pb |> Array.ofList,
    nb |> List.choose (fun arg -> 
      match arg.Kind with
      | GammaEntity(GammaEntityKind.NamedParam(name, value)) -> Some(name, value)
      | _ ->
          Errors.TypeChecker.nameBasedParamMustBeLast |> addError ctx arg
          None ) |> Map.ofList

  // Match actual arguments with the parameters and report
  // error if non-optional parameter is missing an assignment
  let matchedArguments = 
    parameterTypes |> List.mapi (fun index ma ->
      let arg = 
        if index < positionBased.Length then Some(positionBased.[index]) 
        else Map.tryFind ma.Name nameBased 
      match arg with
      | Some arg -> getGammaType arg, if ma.Static then Some arg else None
      | None when ma.Optional -> ma.Type, None
      | None ->
          Errors.TypeChecker.parameterMissingValue ma.Name |> addError ctx argList
          Type.Any, None)

  // Evalaute arguments of static parameters
  Log.trace("typechecker", "Evaluating arguments of type-level method '%s'", methodName)
  for e in matchedArguments |> Seq.choose snd do e.Value <- ctx.Context.Evaluate e
  Log.trace("typechecker", "Evaluated arguments of '%s': %O", methodName, [| for e in Seq.choose snd matchedArguments -> e.Value |])
  
  let tcargs = matchedArguments |> List.map (function (t, Some e) -> t, Some(e.Value.Value) | (t, _) -> t, None)
  match resultTypeFunc tcargs with
  | Some typ -> return typ
  | None ->   
      Log.trace("typechecker", "Invalid argument type when calling '%s'. Argument types: %O", 
        methodName, (Array.ofList (List.map (fst >> AstOps.formatType) matchedArguments)))
      Errors.TypeChecker.parameterConflict |> addError ctx argList
      return Type.Any }
  
/// Type check entity - assumes that all antecedents of the entity 
/// have been reduced to non-delayed type before
let typeCheckEntity (ctx:Languages.AnalyzerContext<_>) (e:Entity) = 
  match e.Kind with

  // Type check global value reference (from globals) and variable reference (from antecedent)
  | GammaEntity(GammaEntityKind.GlobalValue(name, _)) ->
      if not (ctx.Context.Globals.ContainsKey(name)) then
        Errors.TypeChecker.variableNotInScope name |> addError ctx e
        Type.Any :> Wrattler.Ast.Type
      else
        getType ctx.Context.Globals.[name]

  | GammaEntity(GammaEntityKind.Variable(_, inst)) ->
      getType inst      

  // Member access gets type of a given member, call assumes the called thing was a method
  | GammaEntity(GammaEntityKind.Member(inst, nameEnt & { Kind = GammaEntity(GammaEntityKind.MemberName name) })) ->
      match getGammaType inst with 
      | Type.Any -> Type.Any :> _
      | Type.Object(FindMember name mem) -> 
          e.Meta <- mem.Metadata
          mem.Type :> _
      | Type.Object obj ->
          Errors.TypeChecker.memberMissing name obj.Members |> addError ctx nameEnt
          Type.Any :> _
      | typ ->
          Errors.TypeChecker.notAnObject name typ |> addError ctx inst
          Type.Any :> _

  | GammaEntity(GammaEntityKind.MemberAccess(mem)) ->
      getType mem     

  | GammaEntity(GammaEntityKind.Member(inst, _)) ->
      Log.error("typechecker", "typeCheckEntity: Member access is missing member name!")
      failwith "typeCheckEntity: Member access is missing member name!"

  | GammaEntity(GammaEntityKind.Call(inst, { Kind = GammaEntity(GammaEntityKind.ArgumentList(ents)) })) ->
      let name = match lastChainElement inst with { Kind = GammaEntity(ent) } -> entityName ent | _ -> ""
      Log.error("typechecker", "typeCheckEntity: Call to %s has not been type-checked in typeCheckEntityAsync!", name)
      failwithf "typeCheckEntity: Call to %s has not been type-checked in typeCheckEntityAsync!" name

  | GammaEntity(GammaEntityKind.Call(inst, _)) ->
      let name = match lastChainElement inst with { Kind = GammaEntity(ent) } -> entityName ent | _ -> ""
      Log.error("typechecker", "typeCheckEntity: Call to %s is missing argument list!", name)
      failwithf "typeCheckEntity: Call to %s is missing argument list!" name

  // Type of placeholder is the type of its body
  | GammaEntity(GammaEntityKind.Placeholder(_, body)) ->      
      getType body

  // Operators and lists depend on the types of operands and elements...
  | GammaEntity(GammaEntityKind.Operator(l, operator, r)) ->      
      let operandTypes = 
        ( match operator with
          | Operator.Equals -> [PrimitiveType.Number; PrimitiveType.String; PrimitiveType.Date; PrimitiveType.Bool]
          | Operator.Plus -> [PrimitiveType.Number; PrimitiveType.String]
          | _ -> [PrimitiveType.Number] ) |> List.map Type.Primitive 
      [l; r] |> List.iteri (fun idx operand ->
        let typ = getGammaType operand
        if operandTypes |> List.forall (fun opt -> not (typesEqual typ opt)) then
          Errors.TypeChecker.numericOperatorExpectsNumbers operator idx operandTypes typ |> addError ctx operand )
      if not (typesEqual (getGammaType l) (getGammaType r)) then
        Errors.TypeChecker.numericOperatorMismatch operator (getGammaType l) (getGammaType r) |> addError ctx e 
      match operator with
      | Operator.Equals | Operator.LessThan | Operator.GreaterThan 
      | Operator.LessThanOrEqual | Operator.GreaterThanOrEqual -> Type.Primitive PrimitiveType.Bool :> _    
      | _ -> getType l

  | GammaEntity(GammaEntityKind.List(elems)) ->      
      let typs = elems |> List.map getGammaType
      let typ = inferListType typs 
      for a in elems do 
        let elty = getGammaType a
        if not (typesEqual typ elty) then
          Errors.TypeChecker.listElementTypeDoesNotMatch typ elty |> addError ctx a
      Type.List(typ) :> _

  | GammaEntity(GammaEntityKind.Binding(name, { Kind = GammaEntity(GammaEntityKind.CallSite(inst, parSpec)) })) ->
      // Binding node is used to resolve type of a lambda function variable. 
      // Its antecedent is `GammaEntity(GammaEntityKind.CallSite` containing reference to the method around it - 
      // assuming lambda appears in something like: `foo(10, fun x -> ...)`
      match resolveParameterType (getGammaType inst) parSpec with
      | Type.Method([ma], _) -> ma.Type :> _
      | _ -> failwith "typeCheckEntity: Expected parameter of function type"

  | GammaEntity(GammaEntityKind.Binding(name, _)) ->
      failwithf "typeCheckEntity: Variable binding %s is missing call site!" name

  | GammaEntity(GammaEntityKind.Function(var, body)) ->
      let resTyp = getGammaType body
      Type.Method([ { MethodArgument.Name = ""; Optional = false; Static = false; Type = getGammaType var }], fun _ -> Some resTyp) :> _

  // Entities with primitive types
  | GammaEntity(GammaEntityKind.Constant(Constant.Number _)) -> Type.Primitive(PrimitiveType.Number) :> _
  | GammaEntity(GammaEntityKind.Constant(Constant.String _)) -> Type.Primitive(PrimitiveType.String) :> _
  | GammaEntity(GammaEntityKind.Constant(Constant.Boolean _)) -> Type.Primitive(PrimitiveType.Bool) :> _
  | GammaEntity(GammaEntityKind.Constant(Constant.Empty)) -> Type.Any :> _

  // Entities that do not have a real type
  | GammaEntity(GammaEntityKind.LetCommand _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.RunCommand _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.ArgumentList _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.NamedParam _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.CallSite _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.Program _) -> Type.Any :> _
  | GammaEntity(GammaEntityKind.MemberName _) -> Type.Any :> _
  | NonGammaEntity _ -> Type.Any :> _


/// Perform type applications & evaluate delayed types
let rec evaluateDelayedType (t:Type) = async {
  match t with
  | Type.Delayed(f) ->
      let! t = Async.AwaitFuture f
      return! evaluateDelayedType t
  | t -> return t }

/// Type check entity & return its type. This first recursively processes
/// all antecedants to make sure that no antecedant is delayed  
/// (this way, `getType` can be ordinary synchronouus function)
let typeCheckEntityAsync (ctx:Languages.AnalyzerContext<CheckingContext>) (e:Entity) = async {
  match e.Kind with
  | GammaEntity(GammaEntityKind.Call({ Kind = GammaEntity instName } as inst, { Kind = GammaEntity(GammaEntityKind.ArgumentList(ents)) } & arglist)) ->
      let! typ = 
        match getGammaType inst with 
        | Type.Any -> async.Return Type.Any
        | Type.Method(parameterTypes, resultTypeFunc) ->  
            checkMethodCallAsync (entityName instName) ctx parameterTypes resultTypeFunc arglist ents
        | typ ->
            let lastName = match lastChainElement inst with { Kind = GammaEntity ent } -> entityName ent | _ -> ""
            Errors.TypeChecker.notAnMethod lastName typ |> addError ctx inst
            async.Return Type.Any
      return typ :> Wrattler.Ast.Type
  | _ -> 
      for ant in e.Antecedents do
        if ant.Type.IsNone then 
          do! Async.Ignore(ctx.Analyze(ant, ctx.Context))
      return typeCheckEntity ctx e }


// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

let collectTypeErrors (entity:Entity) = 
  let errors = ResizeArray<_>()
  let visited = Dictionary<Symbol, bool>()
  let rec loop (e:Entity) = 
    if not (visited.ContainsKey e.Symbol) then
      visited.[e.Symbol] <- true
      for e in e.Antecedents do loop e
      errors.AddRange(e.Errors)
  loop entity
  errors.ToArray()

let typeCheckProgram ctx prog = async {
  Log.trace("typechecker", "Type checking program")
  try
    let! _ = typeCheckEntityAsync ctx prog 
    Log.trace("typechecker", "Completed type checking")
  with e ->
    Log.exn("typechecker", "Type checking program failed: %O", e) }
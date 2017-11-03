// ------------------------------------------------------------------------------------------------
// Interpreter is used to partially evaluate parts of program as needed
// ------------------------------------------------------------------------------------------------
module Wrattler.Gamma.Interpreter

open Wrattler
open Wrattler.Ast
open Wrattler.Ast.AstOps
open Wrattler.Common
open Wrattler.Gamma.Ast
open Wrattler.Gamma.AstOps
open Fable.Import.Babel
open Fable.Import.Babel.BabelOperators
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Wrappers around `eval` that let us treat runtime values as `Expressions` we can pass to babel
// ------------------------------------------------------------------------------------------------

/// Creates an array of objects and list of expressions that refer
/// to them as if they were stored in an array, e.g. `_stored[0]` and `_stored[1]`
let storeArguments (values:Value list) =
  values |> Array.ofList |> Array.map (function CustomValue v -> v | _ -> null), 
  values |> List.mapi (fun i _ ->
    MemberExpression
      ( IdentifierExpression("_stored", None),
        NumericLiteral(float i, None), true, None ))

/// Evalaute Babel expression, assuming `_stored` is in scope
let evaluateExpression (_stored:obj[]) (expr:Expression) =
  let prog = { Program.location = None; Program.body = [ExpressionStatement(expr, None)] }
  let code = Babel.transformFromAst(Serializer.serializeProgram prog, "", { presets = [| "es2015" |] })
  Log.trace("interpreter", "Interpreter evaluating: %O using values %O", code.code, _stored)
  try
    // HACK (1/2): Get fable to reference everything
    (*
    let s = TheGamma.Series.series<int, int>.create(async { return [||] }, "", "", "") 
    TheGamma.TypeProvidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
    TheGamma.TypeProvidersRuntime.trimLeft |> ignore
    TheGamma.TypeProvidersRuntime.convertTupleSequence |> ignore
    TheGamma.GoogleCharts.chart.bar |> ignore
    TheGamma.table<int, int>.create(s) |> ignore
    TheGamma.General.date.now() |> ignore
    TheGamma.Series.series<int, int>.values([| 1 |]) |> ignore    
    TheGamma.placeholder.create("") |> ignore
    TheGamma.Interactive.youguess.line |> ignore
    *)
    // HACK (2/2) The name `_stored` may appear in the generated code!
    _stored.Length |> ignore
    eval(code.code)
  with e ->
    Log.exn("interpreter", "Evaluation failed: %O", e)
    reraise()

/// Store given arguments and evalaute expression
let evaluateExpr args exprBuilder =
  let _stored, args = storeArguments args
  evaluateExpression _stored (exprBuilder args)

/// If the value is object with 'preview' method or property, evaluate it!
let evaluatePreview (ent:Entity) value = 
  None
  (*
  let previewName = "preview"
  Log.trace("interpreter", "Evaluating preview on: %O (%s)", ent, Gamma.AstOps.formatType ent.Type.Value)
  match ent.Type with
  | Some(Type.Object(FindMember previewName mem)) ->       
      // Member access or member access & call, depending on whether the member is a method
      match mem.Type with
      | Type.Method(_, _) -> evaluateExpr [value] (fun inst -> mem.Emitter.Emit(List.head inst) /@/ []) |> Some
      | _ -> evaluateExpr [value] (fun inst -> mem.Emitter.Emit(List.head inst)) |> Some
  | _ -> None
  *)

type EvaluationContext = 
  { Results : ResizeArray<string option * Value> }

// ------------------------------------------------------------------------------------------------
// Recursively walk over entities & evaluate (starting from antecedents)
// ------------------------------------------------------------------------------------------------

let rec evaluateEntity (ctx:Languages.AnalyzerContext<_>) (e:Entity) : Wrattler.Ast.Value = 
  match e.Kind with
  // Constants, variables & global values (using expression stored in GlobalValue entity)
  | GammaEntity(GammaEntityKind.Constant(Constant.Boolean b)) -> CustomValue(unbox b)
  | GammaEntity(GammaEntityKind.Constant(Constant.Number n)) -> CustomValue(unbox n)
  | GammaEntity(GammaEntityKind.Constant(Constant.String s)) -> CustomValue(unbox s)
  | GammaEntity(GammaEntityKind.Constant(Constant.Empty)) -> CustomValue(unbox null)

  | GammaEntity(GammaEntityKind.Variable(_, value)) ->
      getValue value

  | GammaEntity(GammaEntityKind.GlobalValue(name, expr)) ->
      match expr with
      | Some expr -> CustomValue(evaluateExpression [| |] expr)
      | _ -> Nothing

  // Member access and call - method call is member access followed by a call
  | GammaEntity(GammaEntityKind.Member(inst, { Kind = GammaEntity(GammaEntityKind.MemberName(name)) })) ->
      match inst.Type.Value with 
      | GammaType(Type.Object(FindMember name mem)) -> 
          CustomValue(evaluateExpr [getValue inst] (fun inst -> mem.Emitter.Emit(List.head inst)))
      | _ -> Nothing

  | GammaEntity(GammaEntityKind.MemberAccess(mem)) ->
      getValue mem
  
  | GammaEntity(GammaEntityKind.Call(inst, { Kind = GammaEntity(GammaEntityKind.ArgumentList(args)) })) ->
      // Split arguments between index-based and position-based
      let pb = args |> List.takeWhile (function { Kind = GammaEntity(GammaEntityKind.NamedParam _) } -> false | _ -> true)  
      let nb = args |> List.skipWhile (function { Kind = GammaEntity(GammaEntityKind.NamedParam _) } -> false | _ -> true)  

      let positionBased = 
        pb |> List.map (getValue) |> Array.ofList
      let nameBased =   
        nb |> List.choose(function 
          | { Kind = GammaEntity(GammaEntityKind.NamedParam(name, value)) } -> Some(name, getValue value)
          | _ -> None) |> dict

      // Get expected arguments from the method type
      let expectedArgs = 
        match inst.Type.Value with
        | GammaType(Type.Method(args, resTy)) -> args
        | _ -> []

      // Evalate arguments and instance and run the call 
      let pars = expectedArgs |> List.mapi (fun i ma ->
        if i < positionBased.Length then positionBased.[i]
        elif nameBased.ContainsKey(ma.Name) then nameBased.[ma.Name]
        else (unbox null) )

      match inst with 
      | { Kind = GammaEntity(GammaEntityKind.MemberAccess { 
            Kind = GammaEntity(GammaEntityKind.Member(inst, { Kind = GammaEntity(GammaEntityKind.MemberName(n)) })) }) } ->
          let instValue = getValue inst
          match inst.Type with 
          | Some(GammaType(Type.Object(FindMember n mem))) ->
              evaluateExpr (instValue::pars) (fun stored -> mem.Emitter.Emit(List.head stored) /@/ List.tail stored)
          | _ ->
              evaluateExpr (instValue::pars) (fun stored -> ((List.head stored) /?/ str n) /@/ List.tail stored)
      | _ ->
          let instValue = getValue inst
          evaluateExpr (instValue::pars) (fun stored -> List.head stored /@/ List.tail stored)

  | GammaEntity(GammaEntityKind.Member(inst, _)) ->
      Log.error("interpreter", "typeCheckEntity: Member access is missing member name!")
      Nothing
  | GammaEntity(GammaEntityKind.Call(inst, _)) ->
      let name = match lastChainElement inst with { Kind = GammaEntity(ent) } -> entityName ent | _ -> ""
      Log.error("interpreter", "typeCheckEntity: Call to %s is missing argument list!", name)
      Nothing

  // Binary operators - most map to JavaScript except for power, which is a JS function
  | GammaEntity(GammaEntityKind.Operator(l, Operator.Power, r)) ->
      evaluateExpr [getValue l; getValue r] (function 
        | [l; r] -> ident("Math")?pow /@/ [l; r]
        | _ -> failwith "evaluateEntity: Expected two arguments") |> CustomValue

  | GammaEntity(GammaEntityKind.Operator(l, op, r)) ->
      evaluateExpr [getValue l; getValue r] (function 
        | [l; r] -> 
            let op = 
              match op with
              | Operator.Modulo -> BinaryModulus
              | Operator.Equals -> BinaryEqualStrict
              | Operator.Plus -> BinaryPlus
              | Operator.Minus -> BinaryMinus
              | Operator.Multiply -> BinaryMultiply
              | Operator.Divide -> BinaryDivide
              | Operator.GreaterThan -> BinaryGreater
              | Operator.LessThan -> BinaryLess
              | Operator.GreaterThanOrEqual -> BinaryGreaterOrEqual
              | Operator.LessThanOrEqual -> BinaryLessOrEqual
              | Operator.Power -> failwith "evaluateEntity: Power is not a binary operation"
            BinaryExpression(op, l, r, None)
        | _ -> failwith "evaluateEntity: Expected two arguments") |> CustomValue            

  // Other simple language constructs
  | GammaEntity(GammaEntityKind.List(ents)) ->
      evaluateExpr (List.map (getValue) ents) (fun elements ->
        ArrayExpression(elements, None)) |> CustomValue

  | GammaEntity(GammaEntityKind.Placeholder(_, body)) ->
      CustomValue(getValue body)

  // The following entities do not represent anything that has a value      
  | GammaEntity(GammaEntityKind.ArgumentList _)
  | GammaEntity(GammaEntityKind.NamedParam _)
  | GammaEntity(GammaEntityKind.MemberName _)
  | GammaEntity(GammaEntityKind.Binding _)
  | GammaEntity(GammaEntityKind.CallSite _) ->
      Value.Nothing

  | GammaEntity(GammaEntityKind.LetCommand({ Kind = GammaEntity(GammaEntityKind.Variable(n, _)) }, body)) ->
      ctx.Context.Results.Add((Some n, getValue body))
      Value.Nothing

  | GammaEntity(GammaEntityKind.RunCommand body) -> 
      ctx.Context.Results.Add((None, getValue body))
      Value.Nothing

  | GammaEntity(GammaEntityKind.LetCommand _) 
  | GammaEntity(GammaEntityKind.RunCommand _) -> 
      Log.error("interpreter", "Unexpected let or do command structure: %O", e)
      Value.Nothing

  | GammaEntity(GammaEntityKind.Program(ents)) ->
      for ent in ents do ignore(getValue ent)
      Value.Nothing

  | GammaEntity(GammaEntityKind.Function _) ->
      Log.error("interpreter", "Cannot evaluate entity (probably not supported yet): %O", e)
      Value.Nothing

  | NonGammaEntity e ->
      Log.error("interpreter", "Cannot evaluate non-gamma entity: %O", e)
      Value.Nothing

/// Get value assumes that `evaluateEntityTree` evaluated antecedents already
and getValue (e:Entity) = 
  if e.Value.IsNone then Log.error("interpreter", "getValue: Value of entity %O has not been evaluated.", e)
  e.Value.Value

/// Evalaute antecedents (caching them in `visited`) and then evalaute `e`
let evaluateEntityTreeAsync (ctx:Languages.AnalyzerContext<_>) (e:Entity) = async {
  for ant in e.Antecedents do
    if ant.Value.IsNone then 
      do! ctx.Analyze(ant, ctx.Context)
  return evaluateEntity ctx e }
  
// ------------------------------------------------------------------------------------------------
// Public interface - creating global entities and evaluating entities
// ------------------------------------------------------------------------------------------------
(*
let globalEntity name meta typ expr = 
  { Kind = EntityKind.GlobalValue({ Name = name }, expr)
    Symbol = createSymbol()
    Type = Some typ
    Meta = meta
    Value = None
    Errors = [] }
   *)
let evaluate ctx (e:Entity) = async {
  let _, code = Wrattler.Ast.AstOps.entityCodeAndAntecedents e.Kind
  Log.trace("interpreter", "Evaluating entity %s (%O)", code, e.Kind)
  let! res = evaluateEntityTreeAsync ctx e
  Log.trace("interpreter", "Evaluated entity %s (%O) = %O", code, e.Kind, res)
  return res } 

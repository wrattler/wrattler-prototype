module Wrattler.Gamma.Plugin

open Wrattler.Ast
open Wrattler.Binder
open Wrattler.Ast.AstOps
open Wrattler.Common
open Wrattler.Languages
open Wrattler.Gamma
open Wrattler.Gamma.Ast
open Wrattler.Gamma.AstOps
open Wrattler.Gamma.TypeChecker
open Wrattler.Ast.AstOps

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

open Wrattler.Gamma.TypeProviders

let globalEntity name meta typ expr = 
  { Kind = EntityKind.CustomEntity(GammaEntityWrapper(GammaEntityKind.GlobalValue(name, expr)))
    Symbol = createAutoSymbol()
    Type = Some typ
    Meta = meta
    Value = None
    Language = "gamma"
    Errors = [] }

let buildGlobalsTable provideTypes = Async.StartAsNamedFuture "buildGlobalsTable" <| async {
  // We need to pass the lookup function to the code that provides types
  // (because the providers may need to lookup named types), so we define
  // the map as mutable and fill it later.
  let mutable named = Map.empty
  let lookupNamed n = 
    match named.TryFind(n) with
    | Some(r) -> r
    | None -> 
        Log.error("typechecker", "Could not find named type '%s'", n)
        failwith (sprintf "Could not find named type '%s'" n)

  let! provided = provideTypes lookupNamed
  let allTypes = 
    [ // Pretend we support these - the names appear in the F# provided types
      // and if the functions are not actually used, providing Any type works 
      yield TypeProviders.NamedType("value", Type.Any)
      yield TypeProviders.NamedType("object", Type.Any)
      yield TypeProviders.NamedType("seq", Type.Any) 
      yield TypeProviders.NamedType("async", Type.Any) 
      yield! provided ]

  // Build lookup table from named types and
  // list of global entities (provided global values)
  named <- 
    allTypes
    |> Seq.choose (function TypeProviders.NamedType(s, t) -> Some(s, t) | _ -> None)
    |> Map.ofSeq
  let globalEntities = allTypes |> List.choose (function 
    | TypeProviders.GlobalValue(n, m, e, t) -> 
        Some(globalEntity n m t (Some e))
    | _ -> None)
    (*
  let test = Interpreter.globalEntity "magic" [] (Type.Method(fun vs ->
    { new ObjectType with
        member x.Members = 
          [| for i in 1 .. unbox (List.item 1 vs) -> 
               { Name = unbox (List.head vs) + " " + string i; Type = Type.Primitive(PrimitiveType.String);
                 Metadata = []; Emitter = { Emit = fun _ -> Babel.StringLiteral(unbox (List.head vs), None) } } |]
        member x.TypeEquals _ = false } |> Type.Object |> Some )) (Some(Babel.StringLiteral("test", None)))
        *)
  return globalEntities } 

let rec resolveProvider lookup ignoreFilter kind endpoint = 
  match kind with
  | "rest" ->
      match TypeProviders.RestProvider.provideRestType lookup (resolveProvider lookup ignoreFilter) "anonymous" endpoint "" with
      | ProvidedType.GlobalValue(_, _, e, t) -> t, { Emit = fun _ -> e }
      | _ -> failwith "resolveProvider: Expected global value"
  | "pivot" ->
      let pivotType = async {
        let! typ = TypeProviders.Pivot.providePivotType endpoint ignoreFilter "anonymous" lookup
        match typ with 
        | ProvidedType.GlobalValue(_, _, _, t) -> return t 
        | _ -> return failwith "resolveProvider: Expected global value" }
      Type.Delayed(Async.StartAsNamedFuture ("pivotType:" + endpoint) pivotType),
      { Emit = fun _ -> TypeProviders.Pivot.makePivotExpression endpoint }
  | _ ->
    Log.error("providers", "Cannot resolve provider '%s' (%s)", kind, endpoint) 
    failwith "resolveProvider: Cannot resolve type provider"

let globals = buildGlobalsTable (fun lookup -> async {
  let! js = 
    TypeProviders.FSharpProvider.provideFSharpTypes
      lookup "https://thegamma.net/lib/thegamma-0.1/libraries.json"
  let wb = 
    TypeProviders.RestProvider.provideRestType 
      lookup (resolveProvider lookup false) "worldbank" "https://thegamma-services.azurewebsites.net/worldbank" ""
  
  let! ol = 
    TypeProviders.Pivot.providePivotType 
      "http://thegamma-services.azurewebsites.net/pdata/olympics" false "olympics" lookup
    
  let dd = 
    TypeProviders.RestProvider.provideRestType 
      lookup (resolveProvider lookup false) "datadiff" "http://localhost:10037/datadiff" "" 
  return js @ [ ol; wb; dd ] })

// ------------------------------------------------------------------------------------------------

type GammaBlockKind(code, program:Program) = 
  member x.Program = program
  interface BlockKind with 
    member x.Language = "gamma"
  interface CodeBlock with
    member x.Code = code
    member x.WithCode(newCode) = 
      let newProgram, errors = Parser.parseProgram newCode
      GammaBlockKind(newCode, newProgram) :> _, List.ofArray errors

let gammaChecker = 
  { new Analyzer<BindingResult, Wrattler.Ast.Type, _> with
      member x.CreateContext(bound) = async {
        let rangeLookup = dict [ for r, e in bound.Entities -> e.Symbol, r ]
        let getName = function 
          | { Kind = GammaEntity(GammaEntityKind.GlobalValue(n, _)) } -> n
          | _ -> failwith "getName: Not gamma entity"
        let! globals = Async.AwaitFuture globals
        let globals = Map.ofList [ for e in globals -> getName e, e ]
        return { Globals = globals; Ranges = rangeLookup; Evaluate = fun _ -> failwith "GammaEntity: evaluate" }}

      member x.Analyze(ent, ctx) = async {
        match ent with 
        | { Kind = GammaEntity(ge) } ->
            Log.trace("typechecker", "Checking entity '%s'", Wrattler.Ast.AstOps.entityCodeAndAntecedents ent.Kind |> snd)
            let! typ = TypeChecker.typeCheckEntityAsync ctx ent 
            let! typ = TypeChecker.evaluateDelayedType (typ :?> Type)
            Log.trace("typechecker", "Type of entity '%s' is: %s", Wrattler.Ast.AstOps.entityCodeAndAntecedents ent.Kind |> snd, formatType typ)
            return typ :> _

        | { Kind = CodeBlock("gamma", body, _) } ->
            do! ctx.Analyze(body, ctx.Context)
            return Type.Any :> _

        | ent -> 
            return failwithf "GammaLanguage: Wrong entity (tc): %A" ent } }

let gammaInterpreter = 
  { new Analyzer<unit, Wrattler.Ast.Value, _> with
      member x.CreateContext(_) = 
        async.Return { Interpreter.EvaluationContext.Results = ResizeArray<_>() }
      member x.Analyze(ent, ctx) = async {
        match ent with 
        | { Kind = GammaEntity(ge) } ->
            return! Interpreter.evaluate ctx ent
        | { Kind = CodeBlock("gamma", body, _) } ->
            do! ctx.Analyze(body, ctx.Context)
            return body.Value.Value
        | _ -> 
            return failwith "GammaLanguage: Wrong entity (eval)" } }


   
let language = 
  Monaco.setupMonacoServices ()
  { new LanguagePlugin<_, _, _, _> with 
      member x.Interpreter = Some gammaInterpreter      
      member x.TypeChecker = Some gammaChecker
      member x.Editor = 
        Wrattler.Rendering.createStandardEditor (fun (ctx, id, ed) -> 
          let checker code = ctx.TypeCheck(id, code)
          Monaco.configureMonacoEditor ed checker ) |> Some

      member x.Bind(ctx, block) = async {
        let! globals = Async.AwaitFuture globals
        let globalsNondelay = ResizeArray<_>()
        for g in globals do
          match g.Kind with
          | GammaEntity(GammaEntityKind.GlobalValue(n, _)) -> 
              if g.Type <> None then
                let! t = TypeChecker.evaluateDelayedType (g.Type.Value :?> _)
                g.Type <- Some (t :> _)
              globalsNondelay.Add(n, g)
              | _ -> () 
        match block with 
        | :? GammaBlockKind as block ->
            let prog = Binder.bindProgram (Binder.createContext ctx (List.ofSeq globalsNondelay)) block.Program 
            let block = CodeBlock("gamma", prog, []) |> bindEntity ctx "gamma"
            return block, []
        | _ -> return failwith "Gamma.LanguagePlugin.Bind: Expected GammaBlockKind" }

      member x.Parse(code:string) = 
        let program, errors = Parser.parseProgram code
        GammaBlockKind(code, program) :> _, List.ofArray errors }
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
        | { Kind = DataFrame(_, body) } ->
            do! ctx.Analyze(body, ctx.Context)
            return body.Type.Value

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
        if ent.Type.IsNone then failwith "GammaLanguage: Interpreter requires type checking!"
        match ent with 
        | { Kind = DataFrame(_, body) } ->
            do! ctx.Analyze(body, ctx.Context)
            match ent.Type with
            | Some(GammaType(Type.Object(:? Wrattler.Gamma.TypeProviders.FSharpProvider.GenericType as gt))) 
                when gt.TypeDefinition.FullName.EndsWith("/series") -> 
                let s = 
                  match body.Value with 
                  | Some(CustomValue v) -> unbox<TheGamma.Series.series<obj,obj>> v
                  | _ -> failwith "evaluateEntity: Expected value that is a series"
                let! data = Async.AwaitFuture s.data
                let json = 
                  if isObject (snd data.[0]) then Array.map snd data 
                  else data |> Array.map (fun (k, v) -> Fable.Core.JsInterop.createObj ["key", k; "value", v])
                let! url = Wrattler.Datastore.storeFrame ent.Symbol.ID "it" json
                return Value.Frame(url) 
            | _ -> 
                return Value.Nothing 

        | { Kind = GammaEntity(ge) } ->
            return! Interpreter.evaluate ctx ent
        | { Kind = CodeBlock("gamma", body, _) } ->
            do! ctx.Analyze(body, ctx.Context)
            return body.Value.Value
        | _ -> 
            return failwith "GammaLanguage: Wrong entity (eval)" } }

open Wrattler
open Wrattler.Html

[<Fable.Core.Emit("$0.show($1)")>]
let callShow (o:obj) (id:string) : unit = failwith "JS"

let renderGamma (ctx:EditorContext<_>) (state:Rendering.CodeEditorState) entity =
  [ match entity with 
    | { Kind = EntityKind.CodeBlock(_, { Kind = GammaEntity(GammaEntityKind.Program cmds) }, _) } ->
        let selected = state.SelectedVariable
        let listItems = cmds |> List.mapi (fun i cmd ->
          let label, display = 
            match cmd.Kind with 
            | GammaEntity(GammaEntityKind.LetCommand({ Kind = GammaEntity(GammaEntityKind.Variable(v, _)) }, frame, value)) -> v, value
            | GammaEntity(GammaEntityKind.RunCommand(value)) -> "run", value
            | _ -> failwith "renderGamma: Expected let or run command" 
          let id = sprintf "%s-%d" label i
          id, label, if (selected = None && i = 0) || Some id = selected then Some display else None )
        yield h?ul ["class" => "nav nav-pills"] [
          for id, label, display in listItems do
            yield h?li ["class" => "nav-item"] [
              h?a [
                "class" => (if display.IsSome then "nav-link active" else "nav-link")
                "click" =!> fun _ _ -> ctx.Trigger(Rendering.DisplayVariable(id))
                "href" => "#" ] [text label]
            ]
          ]
        yield h?div ["class"=>"thegamma"] [
          for id, _, display in listItems do
            if display.IsSome then
              Log.trace("gui", "Show entity %O", display.Value)
            match display with
            | Some { Value = Some(CustomValue v); Type = Some(GammaType(Type.Object(FindMember "show" mem))) } -> 
                yield h.delayed (id + "output") (text "Loading output...") (fun id -> callShow v id)
            | Some { Value = Some(CustomValue v); Type = Some(GammaType(Type.Object(:? FSharpProvider.GenericType as gt))) } 
                  when gt.TypeDefinition.FullName.EndsWith("/series") -> 
                yield h.delayed (id + "table") (text "Loading output...") (fun id -> TheGamma.table<obj,obj>.create(unbox v).show(id))
            | Some { Value = None } -> 
                yield h?p [] [ text "Not evaluated yet..." ]
            | Some display -> 
                yield h?p [] [ text (sprintf "Something else: %O" display) ]
            | _ -> ()
          ]
    | ent ->
        yield h?p [] [ text (sprintf "Not a gamma program. Entity: %A" ent) ] ]

let language = 
  Monaco.setupMonacoServices ()
  { new LanguagePlugin<_, _, _, _> with 
      member x.Interpreter = Some gammaInterpreter      
      member x.TypeChecker = Some gammaChecker
      member x.Editor = 
        Wrattler.Rendering.createStandardEditor renderGamma (fun (ctx, id, ed) -> 
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
            let prog = Binder.bindProgram (Wrattler.Gamma.Binder.createContext ctx (List.ofSeq globalsNondelay)) block.Program 
            let block = CodeBlock("gamma", prog, []) |> bindEntity ctx "gamma"
            match prog.Kind with
            | GammaEntity(GammaEntityKind.Program(cmds)) ->
                let exports = 
                  cmds |> List.choose (function
                    | { Kind = GammaEntity(GammaEntityKind.LetCommand({ Kind = GammaEntity(GammaEntityKind.Variable(v, _)) }, frame, value)) } ->
                      Some(v, frame)
                    | _ -> None)
                return block, exports
            | _ -> return failwith "Gamma.LanguagePlugin.Bind: Expected Program entity"
        | _ -> return failwith "Gamma.LanguagePlugin.Bind: Expected GammaBlockKind" }

      member x.Parse(code:string) = 
        let program, errors = Parser.parseProgram code
        GammaBlockKind(code, program) :> _, List.ofArray errors }
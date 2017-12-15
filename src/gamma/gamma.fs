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
      member x.CreateContext(bound) = 
        let rangeLookup = dict [ for r, e in bound.Entities -> e.Symbol, r ]
        { Globals = dict []; Ranges = rangeLookup; Evaluate = fun _ -> failwith "GammaEntity: evaluate" }

      member x.Analyze(ent, ctx) = async {
        match ent with 
        | { Kind = GammaEntity(ge) } ->
            Log.trace("typechecker", "Checking entity '%s'", Wrattler.Ast.AstOps.formatEntityKind ent.Kind)
            let! typ = TypeChecker.typeCheckEntityAsync ctx ent 
            let! typ = TypeChecker.evaluateDelayedType (typ :?> Type)
            Log.trace("typechecker", "Type of entity '%s' is: %s", Wrattler.Ast.AstOps.formatEntityKind ent.Kind, formatType typ)
            return typ :> _

        | { Kind = CodeBlock("gamma", body, _) } ->
            do! ctx.Analyze(body, ctx.Context)
            return Type.Any :> _

        | ent -> 
            return failwithf "GammaLanguage: Wrong entity (tc): %A" ent } }

let gammaInterpreter = 
  { new Analyzer<unit, Wrattler.Ast.Value, _> with
      member x.CreateContext(_) = 
        { Interpreter.EvaluationContext.Results = ResizeArray<_>() }
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
  { new LanguagePlugin<_, _, _, _> with 
      member x.Interpreter = Some gammaInterpreter      
      member x.TypeChecker = Some gammaChecker
      member x.Editor = Some(Wrattler.Rendering.createStandardEditor ())

      member x.Bind(ctx, block) =
        match block with 
        | :? GammaBlockKind as block ->
            let prog = Binder.bindProgram (Binder.createContext ctx []) block.Program
            let block = CodeBlock("gamma", prog, []) |> bindEntity ctx "gamma"
            async.Return(block, [])
        | _ -> failwith "Gamma.LanguagePlugin.Bind: Expected GammaBlockKind" 

      member x.Parse(code:string) = 
        let program, errors = Parser.parseProgram code
        GammaBlockKind(code, program) :> _, List.ofArray errors }
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

type GammaBlockKind(program:Program) = 
  member x.Program = program
  interface CustomBlockKind with 
    member x.Language = "gamma"

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
        | _ -> 
            return failwith "GammaLanguage: Wrong entity" } }

let gammaInterpreter = 
  { new Analyzer<unit, Wrattler.Ast.Value, _> with
      member x.CreateContext(_) = 
        { Interpreter.EvaluationContext.Results = ResizeArray<_>() }
      member x.Analyze(ent, ctx) = async {
        match ent with 
        | { Kind = GammaEntity(ge) } ->
            return! Interpreter.evaluate ctx ent
        | _ -> 
            return failwith "GammaLanguage: Wrong entity" } }


   
let language = 
  { new LanguagePlugin<_, _> with 
      member x.Interpreter = Some gammaInterpreter      
      member x.TypeChecker = Some gammaChecker

      member x.Bind(ctx, block) =
        match block with 
        | :? GammaBlockKind as block ->
            let prog = Binder.bindProgram (Binder.createContext ctx []) block.Program
            prog, []
        | _ -> failwith "Gamma.LanguagePlugin.Bind: Expected GammaBlockKind" 

      member x.Parse(code:string) = 
        let program, errors = Parser.parseProgram code
        CustomBlock(GammaBlockKind(program)), List.ofArray errors }
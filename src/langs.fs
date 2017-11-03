module Wrattler.Languages

open Wrattler.Ast
open Wrattler.Binder

type GlobalAnalyzerContext = 
  { Contexts : Map<string, AnalyzerContext<obj>> 
    Errors : ResizeArray<Error> }
  static member Create(ctx) = { Errors = ResizeArray<_>(); Contexts = ctx }

and AnalyzerContext<'TData> = 
  abstract Context : 'TData
  abstract GlobalContext : GlobalAnalyzerContext
  abstract Analyze : Entity * 'TData -> Async<unit>

type Analyzer<'TInput, 'TOutput, 'TContext> =
  abstract CreateContext : 'TInput -> 'TContext
  abstract Analyze : Entity * AnalyzerContext<'TContext> -> Async<'TOutput>

type LanguagePlugin<'TCheckingContext, 'TInterpreterContext> = 
  abstract Parse : string -> BlockKind * Error list
  abstract Bind : BindingContext * CustomBlockKind -> Entity * list<string * Entity>
  
  abstract Interpreter : Analyzer<unit, Value, 'TInterpreterContext> option
  abstract TypeChecker : Analyzer<BindingResult, Type, 'TCheckingContext> option
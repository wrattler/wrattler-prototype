module Wrattler.Languages

open Wrattler.Ast
open Wrattler.Ast.AstOps
open Wrattler.Common

/// Represents case of the EntityKind union
type EntityCode = string

type EditorContext<'TEvent> = 
  abstract Refresh : unit -> unit
  abstract Trigger : 'TEvent -> unit

type EditorState<'TState> = 
  { CodeChanged : bool 
    Node : Node<Block>
    State : 'TState }

type Editor<'TEvent, 'TState> = 
  abstract Initialize : Node<Block> -> 'TState
  abstract Render : EditorContext<'TEvent> * 'TState -> list<Html.DomNode>
  abstract Update : 'TEvent * 'TState -> EditorState<'TState>

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

and LanguagePlugin<'TCheckingContext, 'TInterpreterContext, 'TEditorEvent, 'TEditorState> = 
  abstract Parse : string -> BlockKind * Error list
  abstract Bind : BindingContext * BlockKind -> Async<Entity * list<string * Entity>>  
  abstract Interpreter : Analyzer<unit, Value, 'TInterpreterContext> option
  abstract TypeChecker : Analyzer<BindingResult, Type, 'TCheckingContext> option
  abstract Editor : Editor<'TEditorEvent, 'TEditorState> option

/// As we bind, we keep root entity, current scope & variables in scope
and BindingContext = 
  { //GlobalValues : Map<Name, Entity>
    Languages : Map<string, LanguagePlugin<obj, obj, obj, obj>>
    Frames : Map<Name, Entity>  
    Root : Entity    
    Evaluate : Entity -> Async<unit>

    /// Table with previously created entities. This is a mutable mapping from 
    /// list of symbols (antecedent entities) together with entity kind & name
    /// to the actual entity. Antecedents capture dependencies (if dependency 
    /// changed, we need to recreate the entity that depends on them)
    Table : ListDictionary<Symbol, Map<EntityCode, Entity>> 
    /// Collects all bound entities and their ranges
    Bound : ResizeArray<Range * Entity> }

/// Represents result of binding syntax tree to entities 
/// (provides access to all bound entities & children lookup function)
and BindingResult(ents:(Range * Entity)[]) = 
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


type CodeBlock = 
  inherit BlockKind
  abstract Code : string
  abstract WithCode : string -> CodeBlock

type MarkdownBlock = 
  { Parsed : obj list }
  interface BlockKind with
    member x.Language = "markdown"


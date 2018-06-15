﻿module Wrattler.Languages

open Wrattler.Ast
open Wrattler.Ast.AstOps
open Wrattler.Common

/// Represents case of the EntityKind union
type EntityCode = string

type EditorContext<'TEvent> = 
  abstract Refresh : unit -> unit
  abstract Trigger : 'TEvent -> unit
  abstract TypeCheck : string * string -> Async<BindingResult>
  abstract Bound : BindingResult
  abstract Selected : bool

and EditorState<'TState> = 
  { StartEvaluation : bool option
    Node : Node<Block>
    State : 'TState }

and Editor<'TEvent, 'TState> = 
  abstract Initialize : Node<Block> -> 'TState
  abstract Render : string * EditorContext<'TEvent> * 'TState -> Html.DomNode
  abstract Update : 'TEvent * 'TState -> EditorState<'TState>

and GlobalAnalyzerContext = 
  { Contexts : Map<string, AnalyzerContext<obj>> 
    Errors : ResizeArray<Error> }
  static member Create(ctx) = { Errors = ResizeArray<_>(); Contexts = ctx }

and AnalyzerContext<'TData> = 
  abstract Context : 'TData
  abstract GlobalContext : GlobalAnalyzerContext
  abstract Analyze : Entity * 'TData -> Async<unit>

and Analyzer<'TInput, 'TOutput, 'TContext> =
  abstract CreateContext : 'TInput -> Async<'TContext>
  abstract Analyze : Entity * AnalyzerContext<'TContext> -> Async<'TOutput>

and LanguagePlugin<'TCheckingContext, 'TInterpreterContext, 'TEditorEvent, 'TEditorState> = 
  abstract Parse : string * string -> BlockKind * Error list
  abstract Bind : BindingContext * BlockKind -> Async<Entity * list<string * Entity>>  
  abstract Interpreter : Analyzer<unit, string * Value, 'TInterpreterContext> option
  abstract TypeChecker : Analyzer<TypeCheckingContext, Type, 'TCheckingContext> option
  abstract Editor : Editor<'TEditorEvent, 'TEditorState> option

and TypeCheckingContext =
  abstract Bound : BindingResult 
  abstract Evaluate : Entity -> Async<unit>

/// As we bind, we keep root entity, current scope & variables in scope
and BindingContext = 
  { //GlobalValues : Map<Name, Entity>
    Languages : Map<string, LanguagePlugin<obj, obj, obj, obj>>
    Frames : Map<Name, Entity>  
    Root : Entity    
    
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
  abstract WithCode : string -> CodeBlock * Error list

open Fable.Import

let (|MarkdownNode|_|) name (tree:obj) = 
  if isArray tree then 
    let tree = unbox<obj[]> tree
    if tree.Length > 0 && isString tree.[0] && unbox<string> tree.[0] = name then 
      Some (List.tail (List.ofArray tree))
    else None
  else None  

type MarkdownBlock = 
  { Source : string; Parsed : obj list }
  interface BlockKind with
    member x.Language = "markdown"
  interface CodeBlock with
    member x.Code = x.Source
    member x.WithCode newCode = 
      let nodes = match Markdown.markdown.parse(newCode) with MarkdownNode "markdown" body -> body | _ -> failwith "No nodes"
      { Source = newCode; Parsed = nodes } :> _, []


namespace Wrattler.Ast
open Wrattler.Common

// ------------------------------------------------------------------------------------------------

type Range = 
  { Block : string
    Start : int
    End : int }

type Error =
  { Number : int
    Message : string
    Range : Range }

type Value = 
  | Nothing
  | Outputs of (string -> unit)[]
  | Frame of string
  | Frames of Map<string, string>

type Name = string

type EntityKind = 
  | Root 
  | Code of lang:string * code:string * frames:Entity list
  | DataFrame of var:string * rblock:Entity
  | CodeBlock of lang:string * code:Entity * vars:Entity list
  | Notebook of blocks:Entity list

and Entity = 
  { Kind : EntityKind
    Symbol : Symbol
    mutable Errors : Error list
    mutable Value : Value option }

type ParsedCode = 
  | RSource of string 
  | JsSource of string

type BlockKind = 
  | CodeBlock of code:ParsedCode
  | MarkdownBlock of obj list

type Block = 
  { Symbol : string 
    BlockKind : BlockKind }

type Node<'T> = 
  { Node : 'T
    mutable Entity : Entity option }

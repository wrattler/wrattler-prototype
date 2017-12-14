namespace Wrattler.Ast
open Wrattler.Common

// ------------------------------------------------------------------------------------------------

type Range = 
  { //Block : string
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
  | CustomValue of obj

type Name = string

type Metadata = 
  { Context : string
    Type : string
    Data : obj }

type Type = interface end

type CustomEntityKind = 
  abstract Language : string
  abstract FormatEntity : unit -> string
  abstract GetCodeAndAntecedents : unit -> Entity list * string

and EntityKind = 
  | Root 
  | Code of lang:string * code:string * frames:Entity list
  | DataFrame of var:string * rblock:Entity
  | CodeBlock of lang:string * code:Entity * vars:Entity list
  | Notebook of blocks:Entity list
  | CustomEntity of CustomEntityKind

and Entity = 
  { Kind : EntityKind
    Symbol : Symbol
    Language : string
    mutable Meta : Metadata list
    mutable Type : Type option
    mutable Errors : Error list
    mutable Value : Value option }

type BlockKind = 
  abstract Language : string

type Block = 
  { ID : string 
    Errors : Error list
    BlockKind : BlockKind }

type Node<'T> = 
  { Node : 'T
    Range : Range
    mutable Entity : Entity option }

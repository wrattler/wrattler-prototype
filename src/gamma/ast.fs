module Wrattler.Gamma.Ast

open Wrattler.Ast
open Wrattler.Common
open Fable.Import

// ------------------------------------------------------------------------------------------------
// Tokens and common 
// ------------------------------------------------------------------------------------------------

/// Binary operators (Equals is tokenized as separate token, but after parsing it can be operator)
type [<RequireQualifiedAccess>] Operator = 
  | Equals
  | Modulo
  | Plus
  | Minus
  | Multiply
  | Divide
  | Power
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual

/// Tokens produced by tokenizer
type [<RequireQualifiedAccess>] TokenKind = 
  | LParen
  | RParen
  | Equals
  | Dot
  | Comma
  | Let
  | LSquare
  | RSquare
  | Colon
  | Fun
  | Arrow
  | Operator of Operator
  | Boolean of bool
  | Number of string * float
  | String of string
  | Ident of string
  | QIdent of string
  | White of string
  | Newline
  | Error of char
  | EndOfFile

/// Token with a range
type Token = 
  { Token : TokenKind 
    Range : Range }

// ------------------------------------------------------------------------------------------------
// Types and code generation
// ------------------------------------------------------------------------------------------------

type Emitter = 
  { Emit : Babel.Expression (* Babel.Expression list *) -> Babel.Expression }

type [<RequireQualifiedAccess>] Documentation = 
  | Text of string
  | Details of string * string
  | None 

type Member = 
  { Name : string 
    Type : Type 
    Metadata : Metadata list 
    Emitter : Emitter }

and ObjectType = 
  abstract Members : Member[] 
  abstract TypeEquals : ObjectType -> bool

and [<RequireQualifiedAccess>] PrimitiveType = 
  | Number
  | Date
  | String
  | Bool
  | Unit

and MethodArgument =
  { Name : string
    Optional : bool
    Static : bool
    Type : Type }

and [<RequireQualifiedAccess>] Type =
  | Delayed of Future<Type>
  | Object of ObjectType
  | Primitive of PrimitiveType
  | List of elementType:Type
  | Method of arguments:MethodArgument list * typ:((Type * Value option) list -> Type option) 
  | Any
  interface Wrattler.Ast.Type

// ------------------------------------------------------------------------------------------------
// Entities - binder attaches those to individual constructs in the parsed AST
// ------------------------------------------------------------------------------------------------

/// Represents constants that can appear in the code
/// (We create separate entity for each, so that we can calculate
/// values of entities and not just types)
type [<RequireQualifiedAccess>] Constant = 
  | Number of float
  | String of string
  | Boolean of bool
  | Empty

/// Represents different kinds of entities that we create. Roughhly
/// corresponds to all places in code where something has a name.
type [<RequireQualifiedAccess>] GammaEntityKind = 

  // Entities that represent root node, program and commands
  | Program of commands:Entity list
  | RunCommand of body:Entity
  | LetCommand of variable:Entity * assignment:Entity

  // Standard constructs of the language
  | List of elements:Entity list
  | Constant of Constant
  | Function of variable:Entity * body:Entity
  | Operator of left:Entity * operator:Operator * right:Entity

  /// Reference to a global symbol or a local variable
  | GlobalValue of name:Name * Babel.Expression option 
  | Variable of name:Name * value:Entity

  /// Variable binding in lambda abstraction
  | Binding of name:Name * callSite:Entity
  /// Call site in which a lambda function appears. Marks method reference & argument
  /// (the argument is the name or the index of the parameter in the list)
  | CallSite of instance:Entity * parameter:Choice<string, int>

  /// Represents all arguments passed to method; Antecedants are individual arguments
  /// (a mix of named parameter & ordinary expression entities)
  | ArgumentList of arguments:Entity list
  /// Named param in a call site with an expression assigned to it
  | NamedParam of name:Name * assignment:Entity

  /// Placeholder with its name and the body entity
  | Placeholder of name:Name * body:Entity

  /// Member access and call with arguments (call has member access 
  /// as the instance; second argument of Member is MemberName)
  | Call of instance:Entity * arguments:Entity
  | Member of instance:Entity * name:Entity 
  | MemberAccess of membr:Entity
  | MemberName of name:Name


// ------------------------------------------------------------------------------------------------
// Parsed AST 
// ------------------------------------------------------------------------------------------------

/// Method call argument, optionally with a named
type Argument =
  { Name : Node<Name> option
    Value : Node<Expr> }

/// A program is a list of commands (with range info)
and Program = 
  { Body : Node<Node<Command> list> }

/// Variable binding or an expression
and Command = 
  | Let of Node<Name> * Node<Expr>
  | Expr of Node<Expr>

/// An expression (does not include let binding, which is a command)
and [<RequireQualifiedAccess>] Expr = 
  | Variable of Node<Name>
  | Member of Node<Expr> * Node<Expr>
  | Call of Node<Expr> * Node<Argument list>
  | Function of Node<Name> * Node<Expr>
  | Placeholder of Node<Name> * Node<Expr>
  | String of string
  | Number of float
  | Boolean of bool
  | Binary of Node<Expr> * Node<Operator> * Node<Expr>
  | List of Node<Expr> list
  | Empty

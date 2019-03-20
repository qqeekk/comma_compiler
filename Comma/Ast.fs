module Ast
open Microsoft.FSharp.Text.Parsing

type Param = string * TypeId
and FunDecl = { name : string; _params : Param list; retType : TypeId; body : Stmt list }
and TypeDecl = { name : string; fields : Param list}
and Decl =
    | FunDecl of FunDecl
    | TypeDecl of TypeDecl
and TypeId = 
    | Single of string
    | Array of string
and Stmt =
    | Empty
    | Expression of Expr
    | VarDecl of Param * Expr
    | Loop of LoopStmt
    | IfElse of Expr * Stmt * Stmt
    | Label of string
    | GoTo of string
    | Return of Expr
    | Break 
    | Continue
    | StmtBlock of Stmt list
and LoopStmt =
    | While of Expr * Stmt
    | DoWhile of Stmt * Expr
and Expr = 
    | Integer of int
    | Boolean of bool
    | Float of float
    | String of string
    | Identifier of string
    | StructInit of string * (string * Expr) list
    | FieldAccess of Expr * string
    | IndexOrArrayInit of Expr * Expr
    | Neg of Expr
    | UMinus of Expr
    | FuncApp of string * (Expr list)
    | Assign of Expr * Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Or of Expr * Expr
    | And of Expr * Expr
    | Equals of Expr * Expr
    | Greater of Expr * Expr
    | GreaterEq of Expr * Expr

exception SyntaxError of ParseErrorContext<obj>

let internal tokenPos (parseState: IParseState) : int -> _ = parseState.InputRange
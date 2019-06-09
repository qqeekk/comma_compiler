module Ast

open FSharp.Text.Lexing
open FSharp.Text.Parsing

type Positions = Position * Position

type InitExpr =
    | StructInit of string * ((string * Positions) * ExprPos) list
    | ArrayInit of string * ExprPos

and AssignableExpr =
    | Identifier of string
    | FieldAccess of ExprPos * string
    | ArrayIndex of ExprPos * ExprPos
and AssignableExprPos = AssignableExpr * Positions

and SideEffectExpr =
    | InitExpr of InitExpr
    | FuncApp of string * (ExprPos list)
    | Assign of AssignableExprPos * ExprPos

and Expr = 
    | Integer of int
    | Boolean of bool
    | Float of float
    | String of string
    | Assignable of AssignableExpr
    | SideEffect of SideEffectExpr
    | Neg of Expr
    | UMinus of Expr
    | Add of ExprPos * ExprPos
    | Sub of ExprPos * ExprPos
    | Mul of ExprPos * ExprPos
    | Div of ExprPos * ExprPos
    | Or of ExprPos * ExprPos
    | And of ExprPos * ExprPos
    | Equals of ExprPos * ExprPos
    | Greater of ExprPos * ExprPos
    | GreaterEq of ExprPos * ExprPos
    | ErrorExp // artifical
and ExprPos = Expr * Positions

type TypeId = 
    | Single of string
    | Array of string
type TypeIdPos = TypeId * Positions

type Param = string * TypeIdPos
type ParamPos = Param * Positions

type Stmt =
    | Empty
    | SideEffect of SideEffectExpr
    | VarDecl of ParamPos * ExprPos
    | Loop of LoopStmt
    | IfElse of ExprPos * StmtPos * StmtPos option
    | Label of string
    | GoTo of string
    | Return of Expr
    | Break
    | Continue
    | StmtBlock of StmtPos list
and StmtPos = Stmt * Positions

and LoopStmt =
    | While of ExprPos * StmtPos
    | DoWhile of StmtPos * ExprPos

type FunSignature = ParamPos list * TypeIdPos

type FunDecl = { 
    name : string
    signature : FunSignature * Positions
    body : StmtPos list 
}

type TypeDecl = { 
    name : string
    fields : ParamPos list
}

type Decl =
    | FunDecl of FunDecl
    | TypeDecl of TypeDecl
type DeclPos = Decl * Positions

type Program = DeclPos list

let internal pos (parseState: IParseState) : int -> _ = 
    parseState.InputRange

let internal inpos (parseState: IParseState) i j = 
    (parseState.InputEndPosition i, parseState.InputStartPosition j)

let internal enpos (parseState: IParseState) i j = 
    (parseState.InputStartPosition i, parseState.InputEndPosition j)

let internal opos (parseState: IParseState) i = 
    inpos parseState (i-1) (i+1)

let internal lpos (parseState: IParseState) i = 
    (parseState.InputStartPosition i, parseState.InputStartPosition (i+1))

let internal rpos (parseState: IParseState) i = 
    (parseState.InputEndPosition (i-1), parseState.InputEndPosition i)

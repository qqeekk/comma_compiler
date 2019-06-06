module CodeGen
open Ast
open LLVMSharp
open NameHelpers

type LLVMCode = string
type LLVMType = string
type LLVMVar = string

let module_ = LLVMModuleRef()

module LLVMNameTracker =
    open System.Collections.Generic

    let private names = Dictionary<string, int>()
    let reset = names.Clear

    let getVar name : LLVMVar =
        sprintf "%%%s%d" name names.[name]

    let addVar name : LLVMVar =
        names.[name] <- snd (names.TryGetValue name) + 1
        getVar name
    
    let makePtrName name : LLVMVar =
        name + ".ptr"

    let makeStructTyName : string -> LLVMType = 
        (+) "%struct."
    
    let makeFuncName : string -> LLVMVar = 
        (+) "@"

    let makeFunParam : LLVMType -> LLVMVar -> _ = 
        sprintf "%s %s"

module rec LLVMTypeProvider = 
    let makeRef : LLVMType -> LLVMType = 
        sprintf "%s*"

    let mapType ty types =
        match Types.lookup ty types with
        | Ok ty_ -> LLVMTypeProvider.getTypeEntry ty_
        | Error m -> failwith m


    let private getTypeEntry = function
    | Single ty -> getType ty
    | Array ty -> getType ty |> makeRef

    let private getType = function
    | Ty.Int -> "i32"
    | Ty.Float -> "double"
    | Ty.Boolean -> "i1"
    | Ty.String -> makeRef "i8"
    | Ty.Record (name, vars) -> 
        //let argTypes = 
        //    vars
        //    |> Seq.map (function | KeyValue (_, value) -> getTypeEntry value)
        //    |> Seq.toArray
        
        LLVMNameTracker.makeStructTyName name


let newline tab : LLVMCode = 
    if tab then "\n    " else "\n"

let store (type_ : LLVMType) (ptr : LLVMVar) value : LLVMCode =
    sprintf "store %s %s, %s %s" type_ value (LLVMTypeProvider.makeRef type_) ptr

let alloca (type_ : LLVMType) (ptr : LLVMVar) : LLVMCode =
    sprintf "%s = alloca %s" ptr type_

let (+>) : LLVMCode -> _ -> _ = (+)

let codegenExpr (funcs, types) strpool = function
    | Expr.Integer n -> string n
    | Expr.Float d -> string d
    | Expr.Boolean true -> "1"
    | Expr.Boolean false -> "0"

let codegenStmt (funcs, types) = function
    | Ast.Stmt.VarDecl (((name, (ty, _)), _), (expr, _)) ->
        let llvmType = LLVMTypeProvider.mapType ty types
        let ptrName = LLVMNameTracker.addVar name |> LLVMNameTracker.makePtrName

        newline true +> alloca llvmType ptrName
    | _ -> ""

let codegenFuncProto types name (args, (retType, _) as __ : FunSignature) : LLVMCode * _ =
    let llvmArgs = [
        for (name, (ty, _)), _ in args ->
            let llvmName = LLVMNameTracker.addVar name
            let llvmType = LLVMTypeProvider.mapType ty types
            llvmType, llvmName
        ]

    let argDecls = llvmArgs |> List.map (fun p -> p ||> LLVMNameTracker.makeFunParam)

    let retTy = LLVMTypeProvider.mapType retType types
    let llvmFunc = LLVMNameTracker.makeFuncName name

    let paramStr = System.String.Join(", ", argDecls)

    sprintf "define %s %s (%s)" retTy llvmFunc paramStr, llvmArgs

let codegenFunc ((_, types) as env) { name = name; signature = sgn, _; body = body } =
    LLVMNameTracker.reset()

    let code, args = codegenFuncProto types name sgn
    
    code
    +> List.fold (fun code (ty, name) -> 
        code 
        +> newline true +> alloca ty (LLVMNameTracker.makePtrName name)
        +> newline true +> store ty (LLVMNameTracker.makePtrName name) name
    ) " {" args 
    +> List.fold (fun code (stmt, _) -> 
        code +> codegenStmt env stmt
    ) "" body
    +> newline false +> "}"
    +> newline false


let codegenDecl env = function
    | FunDecl decl -> codegenFunc env decl
    | _ -> ""

let codegenProgram env : Program -> LLVMCode =
    List.fold (fun code (decl, _) -> code +> newline false +> codegenDecl env decl ) "; llvm 3.8.1"

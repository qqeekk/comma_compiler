module rec Comma.CodeGen
open Ast
open LCode

let constant = """
@print_str.ptr = private constant [4 x i8] c"%s\0A\00"
@print_bln.ptr = private constant [4 x i8] c"%i\0A\00"
@print_flt.ptr = private constant [4 x i8] c"%f\0A\00"
"""

let footer = """
; library code
define i32 @main(i32, i8**) {
    %main.ret.ptr = alloca i32
    call void @.main(i32* %main.ret.ptr)
    %main.ret = load i32, i32* %main.ret.ptr
    ret i32 %main.ret
}

define void @print_str (i32* %ret.value, i8* %str) {
    %print_str.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_str.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* print_str.ptr, i8* %str)
    strore i32 %ret, i32* %ret.value
    ret void 
}

define void @print_int (i32* %ret.value, i32 %i) {
    %print_int.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* print_int.ptr, i32 %i)
    strore i32 %ret, i32* %ret.value
    ret void 
}

define void @print_flt (i32* %ret.value, double %d) {
    %print_flt.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_flt.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* print_flt.ptr, double %d)
    strore i32 %ret, i32* %ret.value
    ret void 
}

define void @print_bln (i32* %ret.value, i1 %b) {
    %print_bln.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* print_bln.ptr, i1 %b)
    strore i32 %ret, i32* %ret.value
    ret void 
}

declare i32 @printf(i8*, ...)

"""

let codegenAssignable = function
    | Identifier l ->
        let lvar, lty = LVars.get l
        "", LVars.ptr lvar, lty

    | ArrayIndex ((l, _), (r, _)) ->
        let acode, av, aty = codegenExpr l
        let icode, iv, _ = codegenExpr r
        let ety = 
            match aty with
            | Ref ty | LTyEntry.Array (_, ty) -> ty
            | Val _ -> exit 1

        let tempptr = LVars.ptr (LVars.addTemp aty)

        let code =
            nl true +> tempptr %= getelementptr true ety av iv []

        acode +> icode +> code, tempptr, ety

    | FieldAccess ((expr, _), field) -> 
        let code, ptr, ty = codegenExpr expr
        let fnum, fty = Map.find field (LTypes.lmeta ty)
        let basety =
            match ty with
            | Val (STp name) -> Val (ST name)
            | _ -> exit 1
        
        let tempptr = LVars.ptr (LVars.addTemp fty)
        let code' = 
            nl true +> tempptr %= getelementptr false basety ptr "0" [string fnum]
        
        code +> code', tempptr, fty

let rec codegenSideEffect = function
    | Assign ((a, _), (expr, _)) ->
        let ecode, evar, ety = codegenExpr expr
        let acode, avar, _ = codegenAssignable a
        let code' = nl true +> store ety avar evar

        ecode +> acode +> code', evar, ety

    | FuncApp (name, exprs) ->
        let exprs = List.map (fst >> codegenExpr) exprs
        let values = List.map (fun (_, v, ty) -> ty, v) exprs

        let retVTy = LFuncs.tget name
        let retV = LVars.add (name + ".ret") retVTy
        let retVPtr = LVars.ptr retV
 
        let code =
            List.fold (fun code (code', _, _) -> code +> code') "" exprs
            +> nl true +> retVPtr %= alloca retVTy
            +> nl true +> call_void (LFuncs.lname name) ((Ref retVTy, retVPtr)::values)
            +> nl true +> retV %= load retVTy retVPtr

        code, retV, retVTy

    | InitExpr (StructInit (name, assigns)) ->
        let lty = LTypes.lentry (Single name)
        let basety = 
            match lty with
            | Val (STp name) -> Val (ST name)
            | _ -> exit 1

        let meta = LTypes.lmeta lty
        let keyvalues = 
            List.map (fun ((key, _), (expr, _)) -> 
                fst (Map.find key meta), codegenExpr expr
            ) assigns
        
        let allocatedPtr = LVars.ptr (LVars.addTemp basety)

        let code =
            List.fold (fun code (_, (code', _, _)) -> code + code') "" keyvalues
            +> nl true +> allocatedPtr %= alloca basety 
            +> (keyvalues 
            |> List.fold (fun code (fnum, (_, v, vty)) ->
                let tempPtr = LVars.ptr (LVars.addTemp vty)
                code 
                +> nl true +> tempPtr %= getelementptr false basety allocatedPtr "0" [string fnum]
                +> nl true +> store vty tempPtr v
            ) "" )
            
        code, allocatedPtr, lty

    | InitExpr (ArrayInit (typeid, (Integer size, _))) ->
        let basety = LTypes.lentry (Single typeid)
        let arrayType = LTyEntry.Array (size, basety)

        let temp = LVars.addTemp arrayType
        let tempPtr = LVars.ptr temp

        let code =
            nl true +> tempPtr %= alloca arrayType
            +> nl true +> temp %= getelementptr true arrayType tempPtr "0" ["0"]
        
        code, temp, (Ref basety)
        
    | InitExpr (ArrayInit _) -> exit 1

let rec codegenExpr expr : LCode * string * LTyEntry = 
    let codegenBinOp ((l, _), (r, _)) instr =
        let lcode, lval, ltype = codegenExpr l
        let rcode, rval, _ = codegenExpr r
        let temp = LVars.addTemp ltype
        
        let code = nl true +> temp %= binop (instr ltype) lval rval
        lcode +> rcode +> code, temp, ltype

    match expr with
    | Expr.Integer n -> 
        "", string n, Val I32
    
    | Expr.Float d -> 
        "", string d, Val D
    
    | Expr.Boolean b -> 
        let b = if b then 1 else 0
        "", string b, Val I1
    
    | Expr.String str -> 
        let var, len = LStrings.get str
        let strty = LTyEntry.Array (len, Val I8)
        let varty = Val I8p

        let tempVar = LVars.addTemp varty
        let code = nl true +> tempVar %= getelementptr false strty var "0" ["0"]
        code, tempVar, varty
    
    | Expr.Add (l, r) -> 
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 -> "add "
            | Val D -> "fadd "
            | _ -> exit 1        
            + LTypes.stringify t
            
    | Expr.Sub (l, r) -> 
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 as t -> "sub "
            | Val D as t -> "fsub "
            | _ -> exit 1        
            + LTypes.stringify t
    
    | Expr.Mul (l, r) -> 
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 -> "mul " 
            | Val D -> "fmul "
            | _ -> exit 1
            + LTypes.stringify t
    
    | Expr.Div (l, r) -> 
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 -> "div " 
            | Val D -> "fdiv "
            | _ -> exit 1 
            + LTypes.stringify t
    
    | Expr.Or ((l, _), (r, _)) ->
        let lbool = Val I1
        let acc = LVars.addTemp lbool
        let accptr = LVars.ptr acc
        
        let lcode, lval, _ = codegenExpr l
        let lleft = LLabels.add "or.left"
        let lright = LLabels.add "or.right"
        let rcode, rval, _ = codegenExpr r

        let code = 
            nl true +> accptr %= alloca lbool
            +> lcode 
            +> nl true +> store lbool accptr lval
            +> nl true +> brc lval lright lleft
            +> nl true +> nl true +> label lleft
            +> rcode
            +> nl true +> store lbool accptr rval
            +> nl true +> br lright
            +> nl true +> nl true +> label lright
            +> nl true +> acc %= load lbool accptr
        
        code, acc, lbool
    
    | Expr.And ((l, _), (r, _)) ->
        let lbool = Val I1
        let acc = LVars.addTemp lbool
        let accptr = LVars.ptr acc
        
        let lcode, lval, _ = codegenExpr l
        let lleft = LLabels.add "and.left"
        let lright = LLabels.add "and.right"
        let rcode, rval, _ = codegenExpr r

        let code = 
            nl true +> accptr %= alloca lbool
            +> lcode 
            +> nl true +> store lbool accptr lval
            +> nl true +> brc lval lleft lright
            +> nl true +> nl true +> label lleft
            +> rcode
            +> nl true +> store lbool accptr rval
            +> nl true +> br lright
            +> nl true +> nl true +> label lright
            +> nl true +> acc %= load lbool accptr
        
        code, acc, lbool
    
    | Expr.Equals (l, r) ->
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val (I1 | I8p | I32) -> "icmp eq "
            | Val D -> "fcmp oeq "
            | _ -> exit 1
            + LTypes.stringify t

    | Expr.Greater (l, r) ->
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 -> "icmp sgt "
            | Val D -> "fcmp ogt "
            | _ -> exit 1
            + LTypes.stringify t

    | Expr.GreaterEq (l, r) ->
        codegenBinOp (l, r) <| fun t ->
            match t with
            | Val I32 -> "icmp sge "
            | Val D -> "fcmp oge "
            | _ -> exit 1
            + LTypes.stringify t

    | Expr.Neg b ->
        let lcode, v, bty = codegenExpr b
        let temp = LVars.addTemp bty

        let code =
            lcode +> nl true +> temp %= (sprintf "xor %s %s, 1" (LTypes.stringify bty) v)

        code, temp, bty

    | Expr.UMinus n ->
        let lcode, v, ty = codegenExpr n
        let temp = LVars.addTemp ty

        let instr = 
            match ty with
            | Val I32 as t -> sprintf "sub %s %s, 0" (LTypes.stringify t) v
            | Val D as t -> sprintf "fsub %s %s, 0.0" (LTypes.stringify t) v
            | _ -> exit 1
        
        lcode +> nl true +> temp %= instr, temp, ty
    
    | Expr.Assignable a ->
        let lcode, v, ty = codegenAssignable a
        let temp = LVars.addTemp ty

        lcode +> nl true +> temp %= load ty v, temp, ty
    
    | Expr.SideEffect se -> 
        codegenSideEffect se

    | ErrorExp -> 
        exit 1    

let rec codegenStmt looplbls retvar = function
    | Stmt.VarDecl (((name, (ty, _)), _), (expr, _)) ->
        let code, value, _ = codegenExpr expr
        
        let llvmType = LTypes.lentry ty
        let ptrName = LVars.ptr (LVars.add name llvmType)
        
        code +> nl true +> ptrName %= alloca llvmType 
        +> nl true +> store llvmType ptrName value
    
    | Stmt.IfElse ((expr, _), (stmt1, _), stmt2) ->
        let expcode, cond, _ = codegenExpr expr
        
        let lthen = LLabels.add "cond.then"
        let lfi = LLabels.add "cond.fi"
        let lelse = 
            let lelse' = LLabels.add "cond.else" 
            if Option.isSome stmt2 then lelse' else lfi

        expcode 
        +> nl true +> brc cond lthen lelse 
        +> nl true +> nl true +> label lthen
        +> codegenStmt looplbls retvar stmt1
        +> nl true +> br lfi
        +> 
        match stmt2 with
        | Some (stmt, _) ->
            nl true +> nl true +> label lelse
            +> codegenStmt looplbls retvar stmt
            +> nl true +> br lfi
        | None -> ""
        +> nl true +> nl true +> label lfi
    
    | Stmt.StmtBlock stmts -> 
        List.fold (fun code (stmt, _) -> code +> codegenStmt looplbls retvar stmt) "" stmts
    
    | Stmt.Label l ->
        nl true +> nl true +> label ("user." + l)
    
    | Stmt.GoTo l ->
        nl true +> br ("user." + l)
    
    | Stmt.Loop (While ((expr, _), (stmt, _))) -> 
        let expcode, cond, _ = codegenExpr expr
        
        let lbegin = LLabels.add "loop.begin"
        let lthen = LLabels.add "loop.then"
        let lend = LLabels.add "loop.end"
        
        nl true +> br lbegin
        +> nl true +> nl true +> label lbegin
        +> expcode 
        +> nl true +> brc cond lthen lend
        +> nl true +> nl true +> label lthen
        +> codegenStmt (Some (lbegin, lend)) retvar stmt
        +> nl true +> br lbegin
        +> nl true +> nl true +> label lend
    
    | Stmt.Loop (DoWhile ((stmt, _), (expr, _))) -> 
        let expcode, cond, _ = codegenExpr expr
        
        let lbegin = LLabels.add "loop.begin"
        let lend = LLabels.add "loop.end"

        nl true +> br lbegin
        +> nl true +> nl true +> label lbegin
        +> codegenStmt (Some (lbegin, lend)) retvar stmt
        +> expcode 
        +> nl true +> brc cond lbegin lend
        +> nl true +> nl true +> label lend
    
    | Stmt.Break ->
        nl true +> br (snd (Option.get looplbls))
    
    | Stmt.Continue ->
        nl true +> br (fst (Option.get looplbls))
    
    | Stmt.Empty -> ""
    
    | Stmt.SideEffect se -> 
        let code, _, _ = codegenSideEffect se
        code

    | Stmt.Return expr ->
        let code, v, ty = codegenExpr expr

        code
        +> nl true +> store ty retvar v
        +> nl true +> "ret void"

let codegenFuncProto name (args, (retType, _) as __ : FunSignature) : LCode * _ * _ =
    let llvmFunc = LFuncs.lname name
    let llvmArgs = 
        [ for (name, (ty, _)), _ in args ->
            let llvmType = LTypes.lentry ty
            let llvmName = LVars.add name llvmType
            llvmType, llvmName
        ]

    let retTy = Ref (LTypes.lentry retType)
    let retName = LVars.add "ret.value" retTy

    let argDecls = (retTy, retName)::llvmArgs |> List.map (fun p -> p ||> LFuncs.makeParam)    
    let paramStr = System.String.Join(", ", argDecls)

    let code = sprintf "define void %s (%s)" llvmFunc paramStr
    code, retName, llvmArgs

let codegenDecl = function
    | FunDecl { name = name; signature = sgn, _; body = body } ->
        do LVars.reset()
        do LLabels.reset()
        let proto, retvar, args = codegenFuncProto name sgn

        proto +> " {" 
        +> nl true +> comment "copy params to locals"
        +> List.fold (fun code (ty, name) -> 
            code 
            +> nl true +> LVars.ptr name %= alloca ty
            +> nl true +> store ty (LVars.ptr name) name
        ) "" args 
        +> snd (List.fold (fun (i, code) (stmt, _) -> 
            let code' = 
                nl true +> nl true +> comment (sprintf "statement %d" i) 
                +> codegenStmt None retvar stmt
        
            i + 1, code + code'
        ) (1, "") body)
        +> nl false +> "}"
        +> nl false

    | TypeDecl { name = name } -> 
        let lty = LTypes.lentry (Single name)
        let basety = 
            match lty with
            | Val (STp name) -> Val (ST name)
            | _ -> exit 1

        let fields = LTypes.lmeta lty

        LTypes.stringify basety %= "type {"
        +> Seq.fold (fun code (KeyValue (_, (_, ty))) -> 
            code +> nl true +> LTypes.stringify ty + "," ) "" fields
        +> nl false +> "}"
        +> nl false

let codegenProgram (program : Program) : LCode =
    LTypes.reset()
    LFuncs.reset()
    LStrings.reset()

    let body = 
        program
        |> List.fold (fun code (decl, _) -> 
            code +> nl false +> codegenDecl decl
        ) (nl false)
    
    let globalScope = 
        LStrings.all()
        |> List.fold (fun code (var, str, len) ->
            code +> nl false +> (conststr var len str)
        ) (nl false) 
   
    comment "llvm 3.8.1" +> globalScope +> constant +> body +> footer

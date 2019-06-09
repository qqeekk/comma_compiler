namespace Comma
open System.Collections.Generic
open NameHelpers

type LCode = string
type LTy = | I8p | I32 | I1 | D | STp of string | ST of string | I8 (* only for llvm use *)
type LTyEntry = 
    | Val of LTy 
    | Ref of LTyEntry 
    | Array of int * LTyEntry

type LVar = string
type LMeta = Map<string, int * LTyEntry>

module LVars =
    let private locals = Dictionary<string, LTyEntry * int>()

    let private llvm name num : LVar =
        sprintf "%%%s%d" name num
    
    let ptr (name : LVar) : LVar =
        name + ".ptr"

    let reset () =
        locals.Clear ()

    let get name =
        let ty, num = locals.[name]
        llvm name num, ty

    let add name ty =
        let num = 
            match locals.TryGetValue name with
            | true, (_, num) -> num
            | _ -> 0

        locals.[name] <- (ty, num + 1)
        fst (get name)

    let addTemp ty : LVar =
        add "." ty
    
    let getTemp () =
        get "."

module LLabels =
    let private labels = Dictionary<string, int>()
    
    let reset () =
        labels.Clear ()

    let llvm = sprintf "label.%s%d" 

    let add name : LVar =
        let num =
            match labels.TryGetValue name with 
            | true, num -> num + 1
            | _ -> 1

        labels.[name] <- num
        llvm name num
   
    //let get name : LVar =
    //    llvm name (labels.[name])

module LStrings =
    let private strpool = Dictionary<string, LVar * int>()
    let mutable private strcount = 0
    
    let private llvm name num : LVar =
        sprintf "@.%s%d" name num

    let reset () = 
        strpool.Clear()
        strcount <- 0

    let get str =
        match strpool.TryGetValue str with
        | true, entry -> entry
        | false, _ ->
            strcount <- strcount + 1
            let entry = llvm "str" strcount, 1 + String.length str
            strpool.[str] <- entry 
            entry
    
    let all () = 
        [for KeyValue (str, (var, len)) in strpool -> (var, str + "\00", len)]

module LTypes =
    let private structs = Dictionary<LTy, Ty>()
    
    let private lname : Ty -> LTy = function
    | Ty.Int -> I32
    | Ty.Float -> D
    | Ty.Boolean -> I1
    | Ty.String -> I8p
    | Ty.Record (name, _) -> STp ("%struct." + name)

    let lentry' : TyEntry -> LTyEntry = function
    | Single ty -> Val (lname ty)
    | TyEntry.Array ty -> Ref (Val (lname ty))

    let private meta : Ty -> LMeta = function
    | Ty.Record (_, fields) ->
        fields
        |> Seq.mapi (fun i (KeyValue (sym, ty)) -> sym, (i, lentry' ty))
        |> Map.ofSeq
    | _ -> 
        Map.empty

    let reset () =
        do structs.Clear ()
        for _, ty in Types.all()
            do structs.[lname ty] <- ty

    let lmeta = function
    | Val st -> structs.[st] |> meta
    | Ref _ | Array _ -> Map.empty

    let lentry = 
        Types.lookup >> function
        | Ok ty -> lentry' ty
        | Error m -> failwith m
    
    let private strigifyTy = function
    | I1 -> "i1"
    | I8 -> "i8"
    | I8p -> "i8*"
    | I32 -> "i32"
    | D -> "double"
    | STp name -> name + "*"
    | ST name -> name

    let rec stringify : LTyEntry -> string = function
    | Val ty -> strigifyTy ty
    | Ref ty -> stringify ty + "*"
    | Array (n, ty) -> sprintf "[%d x %s]" n (stringify ty)

module LFuncs = 
    let private funcs = Dictionary<LVar, LTyEntry>()

    let lname : _ -> LVar = function
    | "main" -> "@.main"
    | name -> "@" + name
    
    let makeParam ty : LVar -> _ = 
        LTypes.stringify ty |> sprintf "%s %s"

    let reset () = 
        do funcs.Clear ()
        for sym, (_, _, rt) in Functions.all() do
            funcs.[lname sym] <- LTypes.lentry' rt
    
    let lget (name : LVar) = funcs.[name]
    let tget (sym : Symbol) = funcs.[lname sym]  

module LCode =
    let comment : string -> LCode = 
        (+) "; "

    let (%=) : LVar -> LCode -> LCode = 
        sprintf "%s = %s"

    let (+>) : LCode -> _ -> _ = (+)
    
    let nl tab : LCode = 
        if tab then "\n    " else "\n"

    let store (type_ : LTyEntry) (ptr : LVar) value : LCode =
        sprintf "store %s %s, %s %s" (LTypes.stringify type_) value (LTypes.stringify (Ref type_)) ptr

    let alloca (type_ : LTyEntry) : LCode =
        sprintf "alloca %s" (LTypes.stringify type_)

    let getelementptr inbounds ty (var : LVar) elementi is : LCode =
        let typtr = LTypes.stringify (Ref ty)
        let trailing_inds = List.fold (fun code -> sprintf ", i32 %s" >> (+) code) "" is

        let instr = if inbounds then "inbounds " else ""
        sprintf "getelementptr %s%s, %s %s, i32 %s%s" 
            instr (LTypes.stringify ty) typtr var elementi trailing_inds

    let conststr var len str : LCode =
        let larr = LTypes.stringify (Array (len, Val I8))
        var %= sprintf "private constant %s c%A" larr str

    let binop opname lval rval : LCode =
        sprintf "%s %s, %s" opname lval rval

    let label : _ -> LCode =
        sprintf "%s:"

    //let load type_ (ptr : LVar) =
    //    sprintf "load %s %s" (LTypes.stringify (Ref type_)) ptr

    let brc cond l1 l2 : LCode =
        sprintf "br i1 %s, label %%%s, label %%%s" cond l1 l2

    let br : _ -> LCode = (+) "br label %"

    let load ty (ptr : LVar) : LCode =
        sprintf "load %s, %s %s" (LTypes.stringify ty) (LTypes.stringify (Ref ty)) ptr
    
    let call_void (name : LVar) args : LCode =
        let params_ = System.String.Join(", ", Seq.map (fun p -> p ||> LFuncs.makeParam) args)
        sprintf "call void %s (%s)" name params_
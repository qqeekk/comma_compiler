namespace Comma
open Ast

type Symbol = string

type Ty = 
    | Int
    | String
    | Boolean
    | Float
    | Record of Symbol * Variables
    override __.ToString() =
        match __ with
        | Int -> "Int"
        | String -> "String"
        | Boolean -> "Bool"
        | Float -> "Float"
        | Record (name, _) -> name

and TyEntry =
    | Single of Ty
    | Array of Ty
    override __.ToString() = 
        match __ with
        | Single ty -> ty.ToString()
        | Array ty -> sprintf "[%O]" ty

and Variables = Map<Symbol, TyEntry>

type Signature = Symbol * TyEntry list * TyEntry
type Functions = Map<Symbol, Signature>
type Labels = Map<Symbol, Variables>
type Types = Map<Symbol, Ty>

[<AutoOpen>]
module TypeChecking = 
    let reportTypeErrorAt = reportErrorAt TypeCheck

module Variables =
    let lookup name (vars: Variables) = 
        match Map.tryFind name vars with
        | Some x -> Ok x
        | None -> Error ("No such variable found: " + name)

    let enter  : _ -> _ -> Variables -> Variables = Map.add
    let default' : Variables = Map.empty

module Labels =
    let lookup name (labels: Labels) = 
        match Map.tryFind name labels with
        | Some venv -> Ok venv
        | None -> Error ("No such label found: " + name)

    let enter: _ -> _ -> Labels -> Labels = Map.add
    let default' : Labels = Map.empty

module Functions =
    let lookup name (funcs: Functions) = 
        match Map.tryFind name funcs with
        | Some name -> Ok name
        | None -> Error ("No such function found: " + name)

    let enter ((name, _, _) as signature) funcs =
        if Map.containsKey name funcs then
            Error (sprintf "Function with name %s is already declared" name)
        else
            Map.add name signature funcs |> Ok
    
    let default' : Functions =
        [ "print_int", [Single Int], Single Int
          "print_str", [Single String], Single Int
          "print_flt", [Single Float], Single Int
          "print_bln", [Single Boolean], Single Int]
        |> List.fold (fun map ty -> match enter ty map with | Ok t -> t | _ -> map) Map.empty

module Types = 
    let lookup typeId (map:Types) = 
        let map trans sym =
            match Map.tryFind sym map with
            | Some ty -> Ok (trans ty)
            | None -> Error ("No such type found: " + sym)
        
        match typeId with
        | Ast.Single ty -> map (Single) ty
        | Ast.Array ty -> map (Array) ty

    let enter ty (types: Types) =
        let key : Symbol = ty.ToString()

        if Map.containsKey key types then
            Error (sprintf "Type %s is already declared" key)
        else
            Ok (Map.add key ty types)

    let default' = 
        [Int; String; Boolean; Float] 
        |> List.fold (fun map ty -> match enter ty map with Ok t -> t | _ -> map) Map.empty
   

module rec TypedAst =
    open FSharp.Text.Lexing

    let expectedTypes (types: Ty seq) = "Expected types: " + (String.concat " | " (Seq.map string types))
    let unmatchedTypes : TyEntry -> TyEntry -> _ = sprintf "Expected type %O but given %O"
    
    type Environment = 
        { labels : Labels 
          types : Types 
          funcs : Functions 
          vars : Variables
          loop : bool
          retType : (TyEntry * bool) option
        }

    let default' =
        { labels = Labels.default'
          types = Types.default'
          funcs = Functions.default'
          vars = Variables.default'
          loop = false
          retType = None
        }
        
    let transParamList types : ParamPos list -> _ = 
        List.choose (fun ((pName, (pTy, pos)), _) -> 
            match Types.lookup pTy types with
            | Ok ty -> 
                Some (pName, ty)
            | Error m -> 
                do reportTypeErrorAt pos m
                None
        )

    let getBinaryExpType env left right =
            getExprType env left, getExprType env right


    let getSideEffectExprType env = function
        | Assign (left, (right, rpos)), _ ->
            let leftTy = getAssignableExprType env left
            let rightTy = getExprType env (right, rpos)

            match leftTy, rightTy with
            | Some x, Some y when x = y -> Some x
            | Some x, Some y -> 
                do reportTypeErrorAt rpos (unmatchedTypes x y)
                None
            | _ -> 
                None

        | FuncApp (name, exprs), pos ->
            match Functions.lookup name (env.funcs) with
            | Ok (_, pars, retType) -> 
                let exprs' = 
                    exprs |> Seq.map (fun (expr, pos) -> getExprType env (expr, pos), pos) 

                try 
                    Seq.iteri2 (fun i pty (ety, epos) ->                        
                        match ety with
                        | Some ety' ->
                            if ety' <> pty then 
                                do reportTypeErrorAt epos (sprintf "Error in parameter %i: " i + unmatchedTypes pty ety')
                        | None -> ()
                    ) pars exprs'
                with _ -> 
                    let plen = Seq.length pars
                    let elen = Seq.length exprs'
                    do reportTypeErrorAt pos (sprintf "Expected %i parameters but given %i" plen elen)

                Some retType
            | Error m -> 
                do reportTypeErrorAt pos m
                None

        | InitExpr (StructInit (name, assocs)), pos ->
            match Types.lookup (TypeId.Single name) (env.types) with
            | Ok (Single (Record (_, vars)) as retType) ->
                Seq.iter (fun ((vname, vpos), (_, epos as exprpos)) -> 
                    let ty = getExprType env exprpos
                    
                    let vty = 
                        match Variables.lookup vname vars with
                        | Ok x -> 
                            Some x
                        | Error m -> 
                            do reportTypeErrorAt vpos m
                            None
                    
                    Option.map2 (fun ty vty ->
                        if ty <> vty then do reportTypeErrorAt epos (unmatchedTypes ty vty)
                    ) ty vty 
                    |> ignore
                ) assocs
                Some retType
            | Ok (Single _ as ty) ->
                do reportTypeErrorAt pos (sprintf "Cannot initialize %O because it is a primitive type" ty)
                Some ty
            | Ok (Array _ as ty) ->
                do reportTypeErrorAt pos (sprintf "Cannot initialize %O because it is an array type" ty)
                Some ty
            | Error m -> 
                do reportTypeErrorAt pos m
                None

        | InitExpr (ArrayInit (typename, size)), pos ->
            match Types.lookup (TypeId.Single typename) (env.types) with
            | Ok (Single ty) when size >= 0 ->
                Some (Array ty)
            | Ok (Single _) -> 
                do reportTypeErrorAt pos "Array size must be a positive integer"
                None
            | Ok (Array _) -> 
                do reportTypeErrorAt pos "Multidimensional arrays are not permitted"
                None
            | Error m -> 
                do reportTypeErrorAt pos m
                None


    let getAssignableExprType env : AssignableExprPos -> _ = function
        | Identifier id, pos -> 
            match Variables.lookup id (env.vars) with
            | Ok n -> 
                Some n
            | Error m ->
                do reportTypeErrorAt pos m
                None

        | ArrayIndex ((_, lpos) as left, ((_, ipos) as ind)), _ ->
            match getBinaryExpType env left ind with
            | Some (Array ty), Some (Single Int) -> 
                Some (Single ty)
            
            | Some (Array ty), Some ity -> 
                do reportTypeErrorAt ipos ("Array indexer error: " + unmatchedTypes (Single Ty.Int) ity)
                Some (Single ty)

            | Some (Single _), _ ->
                do reportTypeErrorAt lpos "Cannot index type which is not an array"
                None
            
            | _ ->
                None
                

        | FieldAccess ((_, epos) as expr, id), pos ->
            match getExprType env expr with
            | Some (Single (Record (_, fields))) -> 
                match Variables.lookup id fields with
                | Ok v -> Some v
                | Error m ->
                    do reportTypeErrorAt pos m
                    None

            | Some ty -> 
                do reportTypeErrorAt epos (sprintf "Cannot access field of type %O" ty)
                None

            | _ -> 
                None
        

    let rec getExprType env : ExprPos -> TyEntry option = function
        | Expr.Assignable expr, pos -> getAssignableExprType env (expr, pos)
        | Expr.SideEffect expr, pos -> getSideEffectExprType env (expr, pos)

        | Add (left, (_, rpos as right)), pos -> 
            match getBinaryExpType env left right with
            | Some (Single (Int | Float | String) as lty), 
              Some (Single (Int | Float | String) as rty) ->
                match lty = rty with
                | true -> Some lty
                | _ -> 
                    do reportTypeErrorAt rpos (unmatchedTypes lty rty)
                    None
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Int; Float; String])
                None
            | _ -> 
                None
        
        | Sub (left, (_, rpos as right)), pos
        | Mul (left, (_, rpos as right)), pos ->
            match getBinaryExpType env left right with
            | Some (Single (Int | Float) as lty), 
              Some (Single (Int | Float) as rty) ->
                match lty = rty with
                | true -> Some lty
                | _ ->
                    do reportTypeErrorAt rpos (unmatchedTypes lty rty)
                    None
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Int; Float])
                None
            | _ -> 
                None
        
        | Div (left, (rexp, rpos as right)), pos ->
            match getBinaryExpType env left right with
            | Some (Single (Int | Float) as lty),
              Some (Single (Int | Float) as rty) ->
                match lty = rty with
                | true when Integer 0 = rexp || Expr.Float 0. = rexp ->
                    do reportTypeErrorAt rpos "Division by 0"
                    Some lty
                | true ->
                    Some lty
                | false ->
                    do reportTypeErrorAt rpos (unmatchedTypes lty rty)
                    None
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Int; Float])
                None
            | _ ->
                None

        | And (left, right), pos
        | Or (left, right), pos ->
            match getBinaryExpType env left right with
            | Some (Single Boolean) as ty, 
              Some (Single Boolean) -> ty
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Boolean])
                None
            | _ -> 
                None

        | Neg expr, pos ->
            match getExprType env (expr, pos) with
            | Some (Single Boolean) as ty -> ty
            | Some ty ->
                do reportTypeErrorAt pos (unmatchedTypes (Single Ty.Boolean) ty)
                None
            | _  -> 
                None
        
        | UMinus expr, pos ->
            match getExprType env (expr, pos) with
            | Some (Single (Int | Float)) as ty -> ty
            | Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Int; Float])
                None
            | _ -> 
                None

        | Expr.Boolean _, _ -> Some (Single Boolean)
        | Expr.Integer _, _ -> Some (Single Int)
        | Expr.Float _, _ -> Some (Single Float)
        | Expr.String _, _ -> Some (Single String)

        | Equals (left, (_, rpos as right)), _ -> 
            match getBinaryExpType env left right with
            | Some x, Some y when x = y -> Some (Single Boolean)
            | Some x, Some y ->
                do reportTypeErrorAt rpos (unmatchedTypes x y) 
                Some (Single Boolean)
            | _ -> 
                None
        
        | ErrorExp, _ -> None // must be handled in parser

        | Greater (left, (_, rpos as right)), pos
        | GreaterEq (left, (_, rpos as right)), pos ->
            match getBinaryExpType env left right with
            | Some (Single (Int | Float) as lty),
              Some (Single (Int | Float) as rty) ->
                match lty = rty with
                | true -> Some (Single Boolean)
                | _ ->
                    do reportTypeErrorAt rpos (unmatchedTypes lty rty)
                    None
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Int; Float])
                None
            | _ -> 
                None


    let transStmt env = function
        | VarDecl (((pname, (ty, typos)), _), expr), pos ->
            let param = 
                match Types.lookup ty (env.types) with 
                | Ok ty -> 
                    Some (pname, ty)
                | Error m -> 
                    do reportTypeErrorAt typos m
                    None

            let right = getExprType env expr
            
            match param, right with
            | Some (name, lty), rty ->
                do rty |> Option.iter (fun rty -> if lty <> rty then reportTypeErrorAt pos (unmatchedTypes lty rty))
                
                { env with vars = Variables.enter name lty (env.vars) }
            
            | _ -> 
                env
        
        | Empty, _ -> env
        | Label l, _ -> 
            { env with labels = Labels.enter l (env.vars) (env.labels) }

        | GoTo l, pos -> 
            match Labels.lookup l (env.labels) with
            | Ok venv -> 
                let varDiff = query { 
                    for v in env.vars do
                    join lv in venv
                        on (v.Key = lv.Key)
                    where (lv.Value <> v.Value)
                    select (v.Key, (lv.Value, v.Value)) 
                }
                
                for sym, (expected, given) in varDiff do 
                    reportTypeErrorAt pos (sprintf "Variables conflict on (%s) in goto statement: " sym 
                                            + unmatchedTypes expected given)

            | Error m -> 
                reportTypeErrorAt pos m
            env

        | IfElse (_, epos as expr, left, right), _ ->
            match getExprType env expr with
            | Some (Single Boolean) -> ()
            | Some ty -> reportTypeErrorAt epos (unmatchedTypes (Single Ty.Boolean) ty)
            | None -> ()
            
            let retType1 = (transStmt env left).retType
            let retType2 = right |> Option.map (transStmt env >> fun env -> env.retType)

            { env with retType = (env.retType, retType1, retType2)
                                 |||> Option.map3 (fun (ty, r) (_, r1) optr2 -> 
                                        let r2 = Option.fold (fun _ -> snd) false optr2
                                        ty, r || (r1 && r2))  }

        | Loop (DoWhile (stmt, (_, epos as expr))), _
        | Loop (While (_, epos as expr, stmt)), _ ->
            match getExprType env expr with
            | Some (Single Boolean) -> ()
            | Some ty -> reportTypeErrorAt epos (unmatchedTypes (Single Ty.Boolean) ty)
            | _ -> ()

            do transStmt { env with loop = true } stmt |> ignore
            env

        | SideEffect se, pos -> 
            do getSideEffectExprType env (se, pos) |> ignore
            env
        
        | StmtBlock stmts, _ ->
            let env' = List.fold (transStmt) env stmts
            { env with retType = (env.retType, env'.retType)
                                ||> Option.map2 (fun (ty, r1) (_, r2) -> ty, r1 || r2) }

        | Break, pos 
        | Continue, pos ->
            if (not env.loop) then 
                reportTypeErrorAt pos "Invalid use of \"break\" | \"continue\" statements" 
            env

        | Return expr, pos -> 
            match env.retType with
            | Some (retType, isReturned) ->
                match getExprType env (expr, pos) with
                | Some ty -> 
                    if retType <> ty then 
                        reportTypeErrorAt pos (unmatchedTypes retType ty)
                | _ -> 
                    ()
                
                if isReturned then
                    do reportTypeErrorAt pos "All code paths have return value before this point"

                { env with retType = Some (retType, true) }

            | None ->
                reportTypeErrorAt pos "Only functions can return values"
                env
            
            
    let transDecl env : DeclPos -> _ = function
        | TypeDecl ty, pos -> 
            let pars' = transParamList (env.types) (ty.fields)
            let venv' = List.fold (fun venv' (name, ty) -> Variables.enter name ty venv') (env.vars) pars'
            
            let record = Record (ty.name, venv')

            match  Types.enter record (env.types) with
            | Ok t ->
                { env with types = t }
            
            | Error m ->
                reportTypeErrorAt pos m
                env

        | FunDecl { signature = (pars, (ty, tyPos)), _; name = name; body = body }, pos -> 
            let pars' = transParamList (env.types) pars
            
            match Types.lookup ty (env.types) with
            | Ok ty -> 
                let pTypes = List.map (snd) pars'

                let venv' = List.fold (fun venv' (name, ty) -> 
                    Variables.enter name ty venv') (env.vars) pars'
                
                let env' = { env with vars = venv'
                                      retType = Some (ty, false) }
                
                let (_, isReturned) = Option.get (List.fold (transStmt) env' body).retType
                
                if not isReturned then 
                    do reportTypeErrorAt pos "Not all code paths return a value"
                
                match Functions.enter (name, pTypes, ty) (env.funcs) with
                | Ok fs -> 
                    { env with funcs = fs }

                | Error m -> 
                    do reportTypeErrorAt pos m
                    env            
            
            | Error m -> 
                do reportTypeErrorAt tyPos m
                env
    
    let transProgram program = 
        let default_pos = Position.Empty, Position.Empty
        
        if List.isEmpty program then 
            reportTypeErrorAt default_pos "Empty program"

        let env = List.fold (transDecl) default' program

        match Functions.lookup "main" env.funcs with
        | Ok (_, [], Single Int) -> ()
        | Ok _ -> reportTypeErrorAt default_pos "main function should have no parameters and Int return type"
        | Error m -> reportTypeErrorAt default_pos m

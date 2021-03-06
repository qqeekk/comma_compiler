﻿namespace Comma
open Ast
open NameHelpers

module rec TypedAst =
    open FSharp.Text.Lexing
    let reportTypeErrorAt = 
        ErrorLogger.reportErrorAt ErrorLogger.Semantics

    let expectedTypes (types: TyEntry seq) = 
        "Expected types: " + (String.concat " | " (Seq.map string types))
    
    let unmatchedTypes : TyEntry -> TyEntry -> _ = 
        sprintf "Expected type %O but there is %O"
    
    type Environment = 
        { labels : Labels
          vars : Variables
          loop : bool
          retType : (TyEntry * bool) option
        }

    let default' =
        { labels = Labels.default'
          vars = Variables.default'
          loop = false
          retType = None
        }
        
    let transParamList params_ = 
        List.choose (fun ((pName, (pTy, pos)), _) -> 
            match Types.lookup pTy with
            | Ok ty -> 
                Some (pName, ty, pos)
            | Error m -> 
                do reportTypeErrorAt pos m
                None
        ) params_

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
            match Functions.lookup name with
            | Ok (_, pars, retType) -> 
                let exprs' = 
                    exprs |> List.map (fun (expr, pos) -> getExprType env (expr, pos), pos) 

                try 
                    List.iteri2 (fun i pty (ety, epos) ->                        
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
            match Types.lookup (TypeId.Single name) with
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

        | InitExpr (ArrayInit (typename, (_, epos as expr))), pos ->
            match Types.lookup (TypeId.Single typename) with
            | Ok (Single ty) ->
                match getExprType env expr with
                | Some (Single Int) -> 
                    match expr with
                    | Integer i, _ when i > 0 -> ()
                    | _ -> reportTypeErrorAt epos "Array size must be compile-stage positive constant"
                
                | Some ty ->
                    reportTypeErrorAt epos (unmatchedTypes (Single Int) ty)
                
                | None -> 
                    reportTypeErrorAt epos (expectedTypes [Single Int])

                Some (Array ty)

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
        | Add (left, (_, rpos as right)), pos
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
                do reportTypeErrorAt pos (expectedTypes [Single Int; Single Float])
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
                do reportTypeErrorAt pos (expectedTypes [Single Int; Single Float])
                None
            | _ ->
                None

        | And (left, right), pos
        | Or (left, right), pos ->
            match getBinaryExpType env left right with
            | Some (Single Boolean) as ty, 
              Some (Single Boolean) -> ty
            | Some _, Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Single Boolean])
                None
            | _ -> 
                None

        | Neg expr, pos ->
            match getExprType env (expr, pos) with
            | Some (Single Boolean) as ty -> ty
            | Some ty ->
                do reportTypeErrorAt pos (unmatchedTypes (Single Boolean) ty)
                None
            | _  -> 
                None
        
        | UMinus expr, pos ->
            match getExprType env (expr, pos) with
            | Some (Single (Int | Float)) as ty -> ty
            | Some _ ->
                do reportTypeErrorAt pos (expectedTypes [Single Int; Single Float])
                None
            | _ -> 
                None

        | Expr.Boolean _, _ -> Some (Single Boolean)
        | Expr.Integer _, _ -> Some (Single Int)
        | Expr.Float _, _ -> Some (Single Float)
        | Expr.String _, _ -> Some (Single String)

        | Equals (left, (_, rpos as right)), pos -> 
            match getBinaryExpType env left right with
            | Some (Single (Int | Float | Boolean | String) as lty), 
              Some (Single (Int | Float | Boolean | String) as rty) when lty = rty 
                -> Some (Single Boolean)
            | Some x, Some y when x = y ->
                do reportTypeErrorAt pos (expectedTypes (List.map Single [Int; Float; Boolean; String] ))
                None
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
                do reportTypeErrorAt pos (expectedTypes [Single Int; Single Float])
                None
            | _ -> 
                None

    let transStmt env = function
        | VarDecl (((pname, (ty, typos)), _), expr), pos ->
            let param = 
                match Types.lookup ty with 
                | Ok ty -> 
                    Some (pname, ty)
                | Error m -> 
                    do reportTypeErrorAt typos m
                    None

            let right = getExprType env expr
            
            match param, right with
            | Some (name, lty), rty ->
                Option.iter (fun rty -> if lty <> rty then reportTypeErrorAt pos (unmatchedTypes lty rty)) rty
                
                let venv =
                    match Variables.enter true name lty (env.vars) with
                    | Ok venv -> venv
                    | _ ->  env.vars // if allowDuplicates = true, no error here.

                { env with vars = venv }
            
            | _ -> 
                env
        
        | Empty, _ -> env
        | Label l, pos -> 
            match Labels.enter l (env.vars) (env.labels) with
            | Ok ls ->
                { env with labels = ls }
            
            | Error m ->
                reportTypeErrorAt pos m
                env

        | GoTo l, pos -> 
            match Labels.lookup l (env.labels) with
            | Ok _ -> ()
            | Error m -> reportTypeErrorAt pos m
            env

        | IfElse (_, epos as expr, left, right), _ ->
            match getExprType env expr with
            | Some (Single Boolean) -> ()
            | Some ty -> reportTypeErrorAt epos (unmatchedTypes (Single Ty.Boolean) ty)
            | None -> ()
            
            let retType1 = (transStmt env left).retType
            let retType2 = right |> Option.map (transStmt env >> fun env -> env.retType) |> Option.flatten

            { env with retType = 
                        (env.retType, retType1)
                        ||> Option.map2 (fun (ty, r) (_, r1) -> 
                            let r2 = 
                                match retType2 with 
                                | Some (_, r2) -> r2 
                                | None -> false
                            
                            ty, r || (r1 && r2)
                        )  
            }

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
                reportTypeErrorAt pos "Use of \"break\" | \"continue\" statements outside of the loop" 
            env

        | Return expr, pos -> 
            match env.retType with
            | Some (retType, _) ->
                match getExprType env (expr, pos) with
                | Some ty -> 
                    if retType <> ty then 
                        reportTypeErrorAt pos (unmatchedTypes retType ty)
                | _ -> 
                    ()
                
                { env with retType = Some (retType, true) }

            | None ->
                reportTypeErrorAt pos "Only functions can return values"
                env
            
    let transDecl : DeclPos -> _ = function
        | TypeDecl ty, pos -> 
            let pars' = transParamList (ty.fields)
            
            let venv' = 
                List.fold (fun venv' (name, ty, pos) -> 
                    match Variables.enter false name ty venv' with
                    | Ok venv -> venv
                    | Error m -> 
                        reportTypeErrorAt pos m
                        venv'
                ) Variables.default' pars'
            
            let record = Record (ty.name, venv')

            match Types.enter record with
            | Ok t -> ()
            | Error m ->
                do reportTypeErrorAt pos m

        | FunDecl { signature = (pars, (ty, tyPos)), _; name = name; body = body }, pos -> 
            let pars' = transParamList pars
            
            match Types.lookup ty with
            | Ok ty -> 
                let pTypes = List.map (fun (_, ty, _) -> ty) pars'

                let venv' = 
                    List.fold (fun venv' (name, ty, pos) -> 
                        match Variables.enter false name ty venv' with
                        | Ok venv -> venv
                        | Error m -> 
                            reportTypeErrorAt pos m
                            venv'
                    ) Variables.default' pars'
            
                let env' = { default' with vars = venv'
                                           retType = Some (ty, false) }
                
                let (_, isReturned) = Option.get (List.fold (transStmt) env' body).retType
                
                if not isReturned then 
                    do reportTypeErrorAt pos "Not all code paths return a value"
                
                match Functions.enter (name, pTypes, ty) with
                | Ok _ -> ()
                | Error m -> do reportTypeErrorAt pos m
            
            | Error m -> do reportTypeErrorAt tyPos m
    
    let transProgram program = 
        let defaultPos = Position.Empty, Position.Empty
        do Types.reset()
        do Functions.reset()

        do List.iter (transDecl) program

        match Functions.lookup "main" with
        | Ok (_, [], Single Int) -> ()
        | Ok _ -> 
            reportTypeErrorAt defaultPos "main function should have no parameters and Int return type"
        | Error m -> 
            reportTypeErrorAt defaultPos m

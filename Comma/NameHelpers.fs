module NameHelpers

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

//[<AutoOpen>]
//module TypeChecking = 
//    let reportTypeErrorAt = 
//        ErrorLogger.reportErrorAt TypeCheck

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

    let enter name env (labels : Labels) = 
        if Map.containsKey name labels then
            Error (sprintf "Label with name %s is already declared in this scope" name)
        else
            Map.add name env labels |> Ok
    
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
        |> List.fold (fun map ty -> 
            match enter ty map with 
            | Ok t -> t 
            | _ -> map
        ) Map.empty

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
        |> List.fold (fun map ty -> 
            match enter ty map with 
            | Ok t -> t 
            | _ -> map
        ) Map.empty
   

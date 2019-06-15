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
type Labels = Map<Symbol, Variables>

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
        | None -> Error ("No such label found above: " + name)

    let enter name env (labels : Labels) = 
        if Map.containsKey name labels then
            Error (sprintf "Label with name %s is already declared in this scope" name)
        else
            Map.add name env labels |> Ok
    
    let default' : Labels = Map.empty

module Functions =
    open System.Collections.Generic
    let private funcs = Dictionary<Symbol, Signature>()

    let lookup name = 
        match funcs.TryGetValue name with
        | true, name -> Ok name
        | false, _ -> Error ("Function is not declared above this point: " + name + " or a recusive call")

    let enter ((name, _, _) as signature) =
        if funcs.ContainsKey name then
            Error (sprintf "Function with name %s is already declared" name)
        else
            Ok (funcs.[name] <- signature)
    
    let reset () =
        do funcs.Clear ()
        do ["print_int", [Single Int], Single Int
            "print_str", [Single String], Single Int
            "print_flt", [Single Float], Single Int
            "print_bln", [Single Boolean], Single Int
            "str_to_int", [Single String], Single Int
            "str_to_flt", [Single String], Single Float
            "input", [], Single String]
        |> List.iter (enter >> ignore)

    let all () =
        funcs |> Seq.map (function KeyValue kvp -> kvp)

module Types = 
    open System.Collections.Generic

    let private types = Dictionary<Symbol, Ty>()

    let enter ty =
        let key : Symbol = ty.ToString()

        if types.ContainsKey key then
            Error (sprintf "Type %s is already declared" key)
        else
            Ok (types.[key] <- ty)

    let lookup typeId = 
        let map trans sym =
            match types.TryGetValue sym with
            | true, ty -> Ok (trans ty)
            | false, _ -> Error ("Type is not declared above this point: " + sym)
        
        match typeId with
        | Ast.Single ty -> map (Single) ty
        | Ast.Array ty -> map (Array) ty

    let reset () = 
        do types.Clear ()
        do List.iter (enter >> ignore) [Int; String; Boolean; Float]
    
    let all () =
        types |> Seq.map (function KeyValue kvp -> kvp)

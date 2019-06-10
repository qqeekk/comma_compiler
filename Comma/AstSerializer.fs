module rec AstSerializer
open System.Xml.Linq
open Ast

let xDeclaration version encoding standalone = 
    XDeclaration (version, encoding, standalone)

let xLocalName localName namespaceName = 
    XName.Get (localName, namespaceName)

let xName expandedName = 
    XName.Get expandedName

let xDocument xdecl (content : XObject seq) = 
    XDocument (xdecl, content |> Seq.map box |> Seq.toArray)

let xComment : string -> _ = XComment

let xElementNS localName namespaceName (content: XObject seq) = 
    XElement(xLocalName localName namespaceName, content |> Seq.toArray) :> XObject

let xElement expandedName (content: XObject seq) = 
    XElement(xName expandedName, Seq.toArray content) :> XObject

let xAttributeNS localName namespaceName value = 
    XAttribute (xLocalName localName namespaceName, value) :> XObject

let xAttribute expandedName value = 
    XAttribute (xName expandedName, value) :> XObject

let xText o = o.ToString() |> XText

let typeValue = function
    | Single ty -> "Single " + ty
    | Array ty -> "Array " + ty

let paramNode ((name, (ty, _)) : Param) = [
        xAttribute "name" name
        xAttribute "type" (typeValue ty)
    ]

let assignableNode = function
    | Identifier n ->
        xElement "id" [xText n]

    | ArrayIndex ((array, _), (ind, _)) ->
        xElement "arrayindex" [
            xElement "value" [exprNode array]
            xElement "index" [exprNode ind]
        ]
    
    | FieldAccess ((expr, _), n) ->
        xElement "fieldaccess" [
            xElement "value" [exprNode expr]
            xElement "field" [xText n]
        ]

let rec exprNode expression = 
    let opName () = (sprintf "%A" expression).Split().[0].ToLower()
    
    match expression with
    | Div ((l, _), (r, _))
    | Mul ((l, _), (r, _))
    | Sub ((l, _), (r, _))
    | Add ((l, _), (r, _))
    | And ((l, _), (r, _))
    | Equals ((l, _), (r, _))
    | Greater ((l, _), (r, _))
    | GreaterEq ((l, _), (r, _))
    | Or ((l, _), (r, _)) ->
        xElement (opName ()) [
            xElement "left" [exprNode l]
            xElement "right" [exprNode r]
        ]

    | Boolean b -> xElement "boolean" [xText b]
    | Integer i -> xElement "integer" [xText i] 
    | String s -> xElement "string" [xText s]
    | Float f -> xElement "float" [xText f]
    | ErrorExp -> xElement "error" []
    | Neg expr -> xElement "neg" [exprNode expr]
    | UMinus expr -> xElement "unaryminus" [exprNode expr]
    
    | Assignable expr -> assignableNode expr
    | Expr.SideEffect expr -> sideEffectNode expr

let sideEffectNode = function
    | Assign ((assignable, _), (expr, _)) ->
        xElement "assign" [
            xElement "name" [assignableNode assignable]
            xElement "value" [exprNode expr]
        ]

    | FuncApp (funcname, args) ->
        xElement "funcall" [
            xElement "name" [xText funcname]
            xElement "args" (Seq.map (fun (arg, _) -> xElement "arg" [exprNode arg]) args)
        ]

    | InitExpr (StructInit (name, assocs)) ->
        xElement "structinit" [
            xElement "name" [xText name]
            xElement "fields" (Seq.map (fun ((fname, _), (expr, _)) -> 
                xElement "field" [
                    xElement "name" [xText fname]
                    xElement "value" [exprNode expr]
                ]) assocs
            )
        ]

    | InitExpr (ArrayInit (name, (size, _))) ->
        xElement "arrayinit" [
            xElement "name" [xText name]
            xElement "size" [exprNode size]
        ]

let rec stmtNode = function
    | Break -> 
        xElement "break" []
    
    | Continue -> 
        xElement "continue" []
    
    | Empty -> 
        xElement "empty" []
    
    | GoTo label -> 
        xElement "goto" [xText label]
    
    | IfElse ((expr, _), (stmt1, _), stmt2) ->
        xElement "ifelse" [
            xElement "conditional" [exprNode expr]
            xElement "onif" [stmtNode stmt1]
            xElement "onelse" (Option.map (fst >> stmtNode) stmt2 |> Option.toList)
        ]

    | Label l ->
        xElement "label" [xText l]
    
    | Loop (While ((expr, _), (body, _))) ->
        xElement "whileloop" [
            xElement "conditional" [exprNode expr]
            xElement "body" [stmtNode body]
        ]

    | Loop (DoWhile ((body, _), (expr, _))) ->
        xElement "dowhileloop" [
            xElement "body" [stmtNode body]
            xElement "conditional" [exprNode expr]
        ]
    
    | Return expr ->
        xElement "return" [exprNode expr]

    | SideEffect expr ->
        xElement "expression" [sideEffectNode expr]
    
    | StmtBlock stmts ->
        xElement "statements" 
            (Seq.map (fst >> stmtNode) stmts)
    
    | VarDecl ((param, _), (expr, _)) ->
        xElement "vardecl" [
            xElement "variable" (paramNode param)
            xElement "value" [exprNode expr]
        ]

let declNode = function
    | FunDecl {name = name; signature = ((pars, (retType, _)), _); body = body} ->
        xElement "fundecl" [
            xElement "name" [xText name]
            xElement "signature" [
                xElement "params" (Seq.map (fst >> paramNode >> xElement "param") pars)
                xElement "returns" [xText <| typeValue retType]
            ]
            xElement "body" (Seq.map (fst >> stmtNode) body)
        ]

    | TypeDecl {name = name; fields = fields } ->
        xElement "typedecl" [
            xElement "name" [xText name]
            xElement "fields" (Seq.map (fst >> paramNode >> xElement "field") fields)
        ]

let serializeToXml program =
    xDocument (xDeclaration "1.0" "UTF-8" "yes") [
        xComment "This document was automatically generated by comma compiler."
        xElement "program" (List.map (fst >> declNode) program)
    ]
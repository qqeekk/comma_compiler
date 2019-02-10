﻿open Microsoft.FSharp.Text.Lexing
open System.Text
open System.IO
open System.Diagnostics
open Parser
open System

let fileExtensions = [| ".cmm" |]
let encoding = Encoding.UTF8

let safeOpenFile filename : Result<FileStream, string> = 
    match File.Exists filename with
    | true -> 
        match Array.contains (Path.GetExtension filename) fileExtensions with
        | true -> 
            try 
                Ok (File.OpenRead filename)
            with _ -> 
                Error ("Cannot open a file: " + filename)
        | false -> 
            Error ("Wrong file format: " + filename)
    | false ->
        Error ("File does not exist: " + filename)


let iterateTokens callback lexbuf =
    // tail-recursive
    let rec loop () =
        match Lexer.tokenize lexbuf with 
        | Eof -> () 
        | x -> do callback lexbuf.EndPos x; loop ()
    
    do loop ()


let compileFromLexbuf lexbuf =
    use file = File.CreateText "lex.output.txt"

    let print (p : Position) tok =
        let str = sprintf "token %A: %A%s" (p.Line + 1, p.Column) tok Environment.NewLine
        
        do Debug.Print str
        do file.Write str

    do iterateTokens print lexbuf


let compileText (text: string) =
    LexBuffer<_>.FromBytes (encoding.GetBytes text) |> compileFromLexbuf
    

let compileFile filename =
    match (safeOpenFile filename) with
    | Ok file ->
        use stream = new BinaryReader(file, encoding)
        do compileFromLexbuf (LexBuffer<_>.FromBinaryReader stream)        
    | Error text -> 
        printfn "%s" text


[<EntryPoint>]
let main = function
    | [| "-f" ; filename |] -> compileFile filename; 0
    | [| text |] -> compileText text; 0
    | _ -> (); 1

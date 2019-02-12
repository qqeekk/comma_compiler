open Microsoft.FSharp.Text.Lexing
open System.Text
open System.IO
open Parser
open Comma.Errors
open Comma.ErrorLogger
open Comma
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

let rec getToken logger (lexbuf: LexBuffer<_>) =
    let lex = Lexer.tokenize lexbuf

    let pos = let ep = lexbuf.EndPos in ep.Line + 1, ep.Column    
    let logToken tok  =  info logger (sprintf "%A: %A" pos tok)
    let logError text = error logger (sprintf "%A: %s" pos text)
    
    match lex with
    | Ok tok     -> logToken tok; tok
    | Error text -> 
        logError text 
        match lexbuf.IsPastEndOfStream with
        | false -> getToken logger lexbuf
        | true  -> logToken Eof; Eof

    
let compileFromLexbuf (lexbuf:LexBuffer<_>) =
    use file = File.CreateText "lex.output.txt"
    let logger = (consoleLogger >=> (* debugLogger >=> *) fileLogger file)

    let rec iterate () =
        match getToken logger lexbuf with 
        | Eof ->         () 
        | _   -> iterate ()

    iterate ()

let compileText (text: string) =
    LexBuffer<_>.FromBytes (encoding.GetBytes text) |> compileFromLexbuf

let compileFile filename =
    match (safeOpenFile filename) with
    | Ok file    ->
        use stream = new BinaryReader(file, encoding)
        do compileFromLexbuf (LexBuffer<_>.FromBinaryReader stream)        
    | Error text -> 
        printfn "%s" text


[<EntryPoint>]
let main = function
    | [| "-f" ; filename |] -> compileFile filename; 0
    | [| text |]            -> compileText text; 0
    | _                     -> printfn "Wrong arguments"; 1

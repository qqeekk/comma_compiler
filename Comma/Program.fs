open Microsoft.FSharp.Text.Lexing
open System.Text
open System.IO
open Parser
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


let iterateTokens callback logger lexbuf =
    // tail-recursive
    let rec loop () =
        match Lexer.tokenize logger lexbuf with 
        | Eof -> () 
        | x -> do callback lexbuf.StartPos lexbuf.EndPos x; loop ()

    do loop ()


let compileFromLexbuf (lexbuf:LexBuffer<_>) =
    use file = File.CreateText "lex.output.txt"
    let logger = (consoleLogger >=> (*debugLogger >=> *) fileLogger file)

    let print (startPos : Position) (endPos : Position) tok =
        let startPos = startPos.Line + 1, startPos.Column
        let endPos = endPos.Line + 1, endPos.Column
        let str = sprintf "token %A" tok

        logger.log (Errors.messageInfo startPos endPos (Errors.InfoMessage str))

    do iterateTokens print logger lexbuf


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

open System.Text
open System.IO
open Comma.ErrorLogger
open FSharp.Text.Lexing
open Comma
open AstSerializer
open System

let fileExtensions = [| ".cmm" |]
let encoding = Encoding.UTF8   

let safeOpenFile filename : Result<FileStream, string> = 
    match File.Exists filename with
    | true -> 
        match Array.contains (Path.GetExtension filename) fileExtensions with
        | true -> 
            try Ok (File.OpenRead filename)
            with _ -> Error ("Cannot open a file: " + filename)
        | _ -> Error ("Wrong file format: " + filename)
    | _ -> Error ("File does not exist: " + filename)


let compileFromLexbuf (lexbuf:LexBuffer<_>) =
    use file = File.CreateText "lex.output.txt"
    let logger = combine [ consoleLogger; debugLogger; fileLogger file ]

    Ast.handleError <- Ast.formatMessage >> error logger
    let parsed = Parser.program (Lexer.getToken logger) lexbuf
    do TypedAst.transProgram parsed
    
    use xmlFile = File.Create "ast.xml"
    (serializeToXml parsed).Save xmlFile


let compileText (text: string) =
    LexBuffer<_>.FromBytes (encoding.GetBytes text) |> compileFromLexbuf


let compileFile filename =
    match (safeOpenFile filename) with
    | Ok file    ->
        use stream = new BinaryReader(file, encoding)
        compileFromLexbuf (LexBuffer<_>.FromBinaryReader stream)
    | Error text -> 
        printfn "%s" text


[<EntryPoint>]
let main _ = 
    while true do
        do printf "> compile -file "
        do Console.ReadLine () |> compileFile
        do printfn ""

    0
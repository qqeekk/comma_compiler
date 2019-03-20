open Microsoft.FSharp.Text.Lexing
open System.Text
open System.IO
open Parser
open Comma.ErrorLogger
open System.Runtime.Serialization
open System.Xml
open MBrace.FsPickler
open MBrace.FsPickler.Combinators

let fileExtensions = [| ".cmm" |]
let encoding = Encoding.UTF8


let serializeToXML o =
    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)
    use writer = new StringWriter ()
    xmlSerializer.Serialize (writer, o)
    writer.ToString()
   

let safeOpenFile filename : Result<FileStream, string> = 
    match File.Exists filename with
    | true -> 
        match Array.contains (Path.GetExtension filename) fileExtensions with
        | true -> 
            try Ok (File.OpenRead filename)
            with _ -> Error ("Cannot open a file: " + filename)
        | false -> Error ("Wrong file format: " + filename)
    | false -> Error ("File does not exist: " + filename)
    

let compileFromLexbuf (lexbuf:LexBuffer<_>) =
    use file = File.CreateText "lex.output.txt"
    let logger = (consoleLogger >=> (* debugLogger >=> *) fileLogger file)

    //iterate ()
    let parsed = Parser.program (Lexer.getToken logger) lexbuf
    info logger (string parsed)
    //info logger (serializeToXML parsed)


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
let main = function
    | [| "-f" ; filename |] -> compileFile filename; 0
    | [| text |]            -> compileText text; 0
    | _                     -> printfn "Wrong arguments"; 1

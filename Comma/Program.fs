open System.Text
open System.IO
open Comma.ErrorLogger
open FSharp.Text.Lexing
open Comma
open AstSerializer
open System
open CodeGen

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
    let parsed = Parser.program (Lexer.getToken) lexbuf
    do TypedAst.transProgram parsed
    parsed
    
let saveAsXml parsed =
    use xmlFile = File.Create "ast.xml"
    let document = serializeToXml parsed
    do document.Save xmlFile

let compileText (text: string) =
    LexBuffer<_>.FromBytes (encoding.GetBytes text) |> compileFromLexbuf

let compileFile =
    safeOpenFile >> Result.map (fun file -> 
        use stream = new BinaryReader(file, encoding)
        compileFromLexbuf (LexBuffer<_>.FromBinaryReader stream)
    )

[<EntryPoint>]
let main _ = 
    let getTreeOrError =
        compileFile >> Result.bind (fun ok ->
            match ErrorLogger.getCompileErrorsTotal() with
            | 0 -> Ok ok
            | n -> Error (sprintf "Compile errors: %d" n)
        )

    let (|ValidArgs|_|) = function
    | [| file |] -> Some (file, false)
    | [| file; "-xml" |] -> Some (file, true)
    | _ -> None

    while true do
        do printf "> compile -file "
        match Console.ReadLine().Split(" ") with
        | [| "\\" |] ->
            exit 0
        | ValidArgs (sfile, toXml) ->
            use file = File.CreateText "compile_session.log"
            do ErrorLogger.resetLogger (combine [ consoleLogger; fileLogger file ])
            
            getTreeOrError sfile |> Result.map (fun tree ->
                if toXml then
                    saveAsXml tree
                    info "AST saved to: ast.xml"
                
                let llvm = codegenProgram tree
                use file = File.CreateText "program.ll"

                file.Write llvm
                info "Compiled source: program.ll"
            )
        | _ ->  
            Error "Wrong args"
        |> Result.mapError (printfn "%s") 
        |> ignore

        do printfn ""
    0

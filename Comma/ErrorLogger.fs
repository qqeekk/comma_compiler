namespace Comma

module Errors =
    type Message = | MError of string | MInfo of string | MTrace of string

    let integerOverflow = "This number is outside the allowable range for 32-bit signed integers"
    let unknownIdentifier = "Invalid symbol or identifier"
    let invalidFloat = "Invalid floating point number"
    let unmatchedDoubleQuote = "Unmatched '\"'"

module ErrorLogger =
    open System
    open Errors
    open System.Diagnostics
    open System.IO
    open Ast
    open FSharp.Text.Lexing

    type CompileStage = Lexing | Parsing | TypeCheck

    type TaggedMessage = TaggedMessage of CompileStage * Positions * string

    let private pretty (pos: Position) = (pos.Line + 1, pos.Column + 1)    

    let formatMessage (TaggedMessage (stage, (pos1, pos2), mes)) =
        sprintf "[%A] %A - %A: %s" stage (pretty pos1) (pretty pos2) mes
 
    type Logger = { log : Message -> unit }

    let combine loggers = 
        { log = fun e -> List.iter (fun l -> l.log e) loggers }
    
    let private format = function 
        | MError m -> sprintf "[Error] %s" m 
        | MInfo m  -> sprintf "[Info] %s" m
        | MTrace m -> sprintf "[Trace] %s" m

    let consoleLogger = 
        let printWithColor color (msg : string) =
            let curColor = Console.ForegroundColor
            Console.ForegroundColor <- color
            printfn "%s" msg
            Console.ForegroundColor <- curColor
        
        { log = function 
            | MError _ as e -> printWithColor ConsoleColor.Red (format e)
            | MInfo _  as i -> printWithColor ConsoleColor.Cyan (format i)
            | MTrace _ as t -> () (*printfn "%s" (format t)*) }

    let debugLogger = 
        { log = format >> Debug.WriteLine }

    let fileLogger (file:TextWriter) = 
        { log = format >> file.WriteLine }

    let mutable private logger = { log = ignore }
    let mutable private totalErrors = 0

    let resetLogger logger' =
        totalErrors <- 0
        logger <- logger'

    let info m = logger.log (MInfo m)
    let error m = logger.log (MError m)
    let trace m = logger.log (MTrace m)
    
    let getCompileErrorsTotal () = 
        totalErrors

    let handleCompileError taggedMessage = 
        formatMessage taggedMessage |> error
        totalErrors <- totalErrors + 1
    
    let reportErrorAt stage range message = 
        handleCompileError (TaggedMessage (stage, range, message))
    

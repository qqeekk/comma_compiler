namespace Comma
open Microsoft.FSharp.Text.Lexing

module Errors =
    type Message = | MError of string | MInfo of string

    let integerOverflow = "This number is outside the allowable range for 32-bit signed integers"
    let unknownIdentifier = "Invalid symbol or identifier"
    let invalidFloat = "Invalid floating point number"
    let unmatchedDoubleQuote = "Unmatched '\"'"

module ErrorLogger =
    open System
    open Errors
    open System.Diagnostics
    open System.IO

    type Logger = { log : Message -> unit }

    let (>=>) logger1 logger2 = {log = fun e -> logger1.log e; logger2.log e}
    
    let format = function 
        | MError m -> sprintf "[Error] %s" m 
        | MInfo m  -> sprintf "[Info] %s" m

    let inline ifDebug (value : Lazy<_>) = 
        #if DEBUG 
            value.Force ()
        #else 
            ()
        #endif

    let consoleLogger = 
        let printWithColor color (msg : string) =
            let curColor = Console.ForegroundColor
            Console.ForegroundColor <- color
            printfn "%s" msg
            Console.ForegroundColor <- curColor
        
        let log = function 
            | MError _ as e -> printWithColor ConsoleColor.Red (format e)
            | MInfo _  as i -> ifDebug (lazy printfn "%s" (format i))

        { log = log }

    let debugLogger = { log = format >> Debug.WriteLine }
    let fileLogger (file:TextWriter) = { log = format >> file.WriteLine }
   
    let info logger = MInfo >> logger.log
    let error logger = MError >> logger.log

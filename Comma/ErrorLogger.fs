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

    type Logger = { log : Message -> unit }

    let combine loggers = 
        { log = fun e -> List.iter (fun l -> l.log e) loggers }
    
    let private format = function 
        | MError m -> sprintf "[Error] %s" m 
        | MInfo m  -> sprintf "[Info] %s" m
        | MTrace m -> sprintf "[Trace] %s" m

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
        
        { log = function 
            | MError _ as e -> printWithColor ConsoleColor.Red (format e)
            | MInfo _  as i -> ifDebug (lazy printWithColor ConsoleColor.Cyan (format i))
            | MTrace _ as t -> () (*ifDebug (lazy printfn "%s" (format t))*) }

    let debugLogger = { log = format >> Debug.WriteLine }
    let fileLogger (file:TextWriter) = { log = format >> file.WriteLine }
   
    let info logger = MInfo >> logger.log
    let error logger = MError >> logger.log
    let trace logger = MTrace >> logger.log

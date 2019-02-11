namespace Comma

module Errors =
    type Position = int * int

    type Message = | ErrorMessage of string | InfoMessage of string
    type MessageInfo = | Error of string | Info of string

    let integerOverflow = 
        ErrorMessage "This number is outside the allowable range for 32-bit signed integers"
    
    let unknownIdentifier = 
        ErrorMessage "Invalid symbol or identifier"
    
    let invalidFloat =
        ErrorMessage "Invalid floating point number"

    let unmatchedDoubleQuote = 
        ErrorMessage "Unmatched '\"'"
    
    let private getPositionRange (startPos: Position) (endPos: Position) =
        let stringify nth = 
            let a, b = nth startPos, nth endPos
            if a = b then string a else sprintf "%d-%d" a b
        
        sprintf "(%s, %s)" (stringify fst) (stringify snd)

    let messageInfo (startPos: Position) (endPos: Position) = function
        | ErrorMessage msg -> Error (sprintf "Error at %s: %s" (getPositionRange startPos endPos) msg)
        | InfoMessage msg -> Info (sprintf "Info at %s: %s" (getPositionRange startPos endPos) msg)

module ErrorLogger =
    open System
    open Errors
    open System.Diagnostics
    open System.IO

    type Logger = { log : MessageInfo -> unit }
    let (>=>) logger1 logger2 = {log = fun e -> logger1.log e; logger2.log e}

    let errorColor = ConsoleColor.Red
    
    let printWithColor color (msg : string) =
        let curColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        Console.WriteLine msg
        Console.ForegroundColor <- curColor

    let consoleLogger = { log = function | Error msg -> printWithColor errorColor msg 
                                         | Info msg -> 
                                            #if DEBUG 
                                                            printfn "%s" msg 
                                            #else 
                                                            ()
                                            #endif
    }
    let debugLogger =   { log = function | Error msg | Info msg -> Debug.Print msg }
    let fileLogger (file:TextWriter) = { log = function | Error msg | Info msg -> file.WriteLine msg }
   

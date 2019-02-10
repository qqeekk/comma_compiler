namespace Comma

module Errors =
    type Position = int * int

    type ErrorMessage = | ErrorMessage of string
    type ErrorInfo = | ErrorInfo of string

    let integerOverflow = 
        ErrorMessage "This number is outside the allowable range for 32-bit signed integers"
    
    let unknownIdentifier = 
        ErrorMessage "Invalid symbol or identifier"
    
    let invalidFloat =
        ErrorMessage "Invalid floating point number"

    let unmatchedDoubleQuote = 
        ErrorMessage "Unmatched '\"'"

    let errorInfo (startPos: Position) (endPos: Position) (ErrorMessage msg) =
        let stringify nth = 
            let a, b = nth startPos, nth endPos
            if a = b then string a else sprintf "%d-%d" a b

        ErrorInfo <| sprintf "Error at (%s, %s): %s" (stringify fst) (stringify snd) msg

module ErrorLogger =
    open System
    open Errors

    type Logger = { error : ErrorInfo -> unit }

    let errorColor = ConsoleColor.Red
    
    let printWithColor color (msg : string) =
        let curColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        Console.WriteLine msg
        Console.ForegroundColor <- curColor

    let consoleLogger = { error = fun (ErrorInfo msg) -> printWithColor errorColor msg }
   

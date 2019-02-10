namespace Comma

module Errors =
    type ErrorMessage = | ErrorMessage of string
    type WarningMessage = | WarningMessage of string

    let integerOverflow = 
        ErrorMessage "This number is outside the allowable range for 32-bit signed integers"
    
    let unknownIdentifier = 
        ErrorMessage "Invalid symbol or identifier"
    
    let invalidFloat =
        ErrorMessage "Invalid floating point number"

    let unmatchedDoubleQuote = 
        ErrorMessage "Unmatched '\"'"

module ErrorLogger =
    open System
    open Errors

    type Position = int * int

    let errorColor = ConsoleColor.Red
    let warningColor = ConsoleColor.Yellow
    
    let printWithColor color (msg : string) =
        let curColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        Console.WriteLine msg
        Console.ForegroundColor <- curColor

    let logLexErrorWithToken (pos: Position) (tok: string) (ErrorMessage msg) = 
        printWithColor errorColor (sprintf "Lexing error at %A : %s %A" pos msg tok)

    let logLexError (startPos: Position) (endPos: Position) (ErrorMessage msg) = 
        printWithColor errorColor (sprintf "Lexing error at %A - %A : %s" startPos endPos msg)

    let logLexWarningWithToken (pos: Position) (tok: string) (WarningMessage msg) =
        printWithColor warningColor (sprintf "Lexing warning at %A : %s %A" pos msg tok)
        
    let logLexWarning (startPos: Position) (endPos: Position) (WarningMessage msg) =
        printWithColor warningColor (sprintf "Lexing warning at %A - %A : %s" startPos endPos msg)


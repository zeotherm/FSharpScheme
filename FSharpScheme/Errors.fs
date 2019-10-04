namespace Lisp

module Errors =
    open Ast
    open Parser

    type LispError =
        | NumArgs of int * LispVal list
        | TypeMismatch of string * LispVal
        | ParseError of string * FParsec.Error.ParserError
        | BadSpecialForm of string * LispVal
        | NotFunction of string * string
        | UnboundVar of string * string
        | Default of string
        | IOError of string

    let showError = function
        | NumArgs(expected, found) -> "Expected " + expected.ToString() + " args; found values " + unwordsList found 
        | TypeMismatch(expected, found) -> "Invalid type: expected " + expected + ", found " + showVal found 
        | ParseError(msg, _) -> "Parse Errror" + msg 
        | BadSpecialForm(message, form) -> message + showVal form 
        | NotFunction(message, func) -> message + func 
        | UnboundVar(message, varName) -> message + varName 
        | Default(message) -> message 
        | IOError(message) -> message 
    
    exception LispException of LispError

    let throw le = raise (LispException(le))

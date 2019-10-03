namespace Lisp
    module Ast =

        type Env = (string * LispVal ref) list ref
        and FuncRecord = {parms: string list; varargs: string option; body: LispVal list; closure: Env}
        and LispVal = 
            | Atom of string
            | List of LispVal list
            | DottedList of LispVal list * LispVal
            | Number of int
            | String of string
            | Bool of bool
            | PrimitiveFunc of (LispVal list -> LispVal)
            | Func of FuncRecord
            | Port of System.IO.FileStream

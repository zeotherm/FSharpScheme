namespace Lisp
module Repl =
    open System
    open Ast
    
    
    let printStr (s : string) = Console.Write(s)
    let readPrompt (s: string) = printStr s ; Console.ReadLine ()
    let newLine () = Console.WriteLine()
    let evalAndPrint env = evalString env >> showVal >> printStr >> newLine


    let evalString env expr =
        try
            expr |> readExpr |> eval env
        with
        | LispException(error) -> String (showError error)


    let rec until pred prompter evaluator =
        let result = prompter ()
        if not (pred result) then
            evaluator result
            until pred prompter evaluator
    
    let loadStdLib env =
        eval env (List [Atom "load"; String "stdlib.scm"]) |> ignore
        env
    
    let runRepl () =
        let env = primitiveBindings () |> loadStdLib
        until (fun s -> s = "Quit" || s = "quit") (fun () -> readPrompt "Lisp>>> ") (evalAndPrint env)

    let runOne (filename : string) (args: string list) =
        let env = primitiveBindings () 
                    |> loadStdLib
                    |> bindVars [ "args", List (List.map String args) ]
        List [Atom "load"; String filename] |> eval env |> showVal |> printStr
   
    let primitiveBindings () =
        (nullenv ()) |> bindVars [ for v, f in primitives -> v, PrimitiveFunc f ]


    [<EntryPoint>]
    let main(args: string[]) = 
        match Array.toList args with 
        | [] -> runRepl ()
        | filename :: args -> runOne filename args
        0

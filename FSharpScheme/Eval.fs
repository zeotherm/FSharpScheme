namespace Lisp

module Eval =

    open Ast
    open Errors
    open System.IO
    open System.Linq.Expressions

    let fileIOFunction func = function 
        | [String fileName] -> func (fileName)
        | [] -> throw (IOError("No file name"))
        | args -> throw (NumArgs(1, args))

    let load = fileIOFunction (fun fileName -> File.ReadAllText(fileName)
                                               |> readExprList)

    let makeFunc varargs env parms body = 
        Func ({parms = (List.map showVal parms); varargs = varargs; body = body; closure = env})
    let makeNormalFunc = makeFunc None
    let makeVarargs = showVal >> Some >> makeFunc

    let foldl1 op = function
        | h :: t -> List.fold op h t
        | [] -> throw (Default("Expected a not empty list, got an empty list"))
    
    let rec unpackNum = function 
        | Number n  -> n 
        | String n  -> match System.Int32.TryParse n with 
                        | (true, v) -> v
                        | (false, _) -> throw (TypeMismatch("number", String n)) 
        | List [n]  -> unpackNum n 
        | notNumber -> throw (TypeMismatch("number", notNumber)) 


    let numericBinop op parms = 
        if List.length parms < 2 then
            throw <| NumArgs(2, parms)
        else
            parms |> List.map unpackNum |> foldl1 op |> Number
    
    let boolBinop unpacker op args = 
        match args with
        | [left; right] -> Bool (op (unpacker left) (unpacker right))
        | _ -> throw (NumArgs(2, args))

    let numBoolBinop = boolBinop unpackNum
    let strBoolBinop = boolBinop unpackStr
    let boolBoolBinop = boolBinop unpackBool

    let rec primitives = 
        [
            "+",    numericBinop (+)
            "-",    numericBinop (-)
            "*",    numericBinop (*)
            "/",    numericBinop (/)
            "mod",  numericBinop (%)
            "=",    numBoolBinop (=)
            "<",    numBoolBinop (<)
            ">",    numBoolBinop (>)
            "/=",   numBoolBinop (<>)
            ">=",   numBoolBinop (>=)
            "<=",   numBoolBinop (<=)
            "&&",   boolBoolBinop (&&)
            "||",   boolBoolBinop (||)
            "string=?",     strBoolBinop (=)
            "string>?",     strBoolBinop (>)
            "string<?",     strBoolBinop (<)
            "string<=?",    strBoolBinop (<=)
            "string>=?",    strBoolBinop (>=)
            "car",  car
            "cdr",  cdr
            "cons", cons
            "eq?",  eqv
            "eqv?", eqv
            "equal?",   equal 
            // IO Primitives
            "apply", applyProc
            "open-input-file", makePort FileAccess.Read
            "open-output-file", makePort FileAccess.Write
            "close-input-port", closePort
            "close-output-port", closePort
            "read", readProc
            "write", writeProc
            "read-contents", readContents
            "read-all", readAll
        ]
    and apply func args =
        match func with 
        | PrimitiveFunc(f) -> f args
        | Func ({parms = parms; varargs = varargs; body = body; closure = closure}) ->
            let invalidNonVarargs = args.Length <> parms.Length && varargs.IsNone
            let invalidVarargs = args.Length < parms.Length && varargs.IsSome
            if invalidVarargs || invalidNonVarargs then
                throw (NumArgs(parms.Length, args))
            else
                let remainingArgs = args |> Seq.skip parms.Length |> Seq.toList
                let evalBody env = body |> List.map (eval env) |> last
                let rec zip xs1 xs2 acc = 
                    match xs1, xs2 with
                    | x1::xs1, x2::xs2 -> zip xs1 xs2 ((x1, x2)::acc)
                    | _ -> acc
                let bindVarArgs arg env =
                    match arg with
                    | Some(argName) -> bindVars [argName, (List remainingArgs)] env
                    | None -> env
                bindVars (zip parms args []) closure
                    |> bindVarArgs varargs 
                    |> evalBody
        | funcName -> throw (NotFunction("Expecting a function, getting ", showVal funcName))
    and eval env = function
    | String _ as v -> v
    | Number _ as v -> v
    | Bool _ as v -> v
    | Atom var -> getVar var env
    | List [Atom "quote"; v] -> v
    | List [Atom "if"; pred; conseq; alt] -> evalIf env pred conseq alt
    | List [Atom "load"; fileName] -> load [fileName] |> List.map (eval env) |> last
    | List [Atom "set!" ; Atom var ; form] -> env |> setVar var (eval env form)
    | List [Atom "define"; Atom var; form] -> define env var (eval env form)
    | List (Atom "define" :: (List (Atom var :: parms):: body)) ->
        makeNormalFunc env parms body |> define env var
    | List (Atom "define" :: (DottedList ((Atom var :: parms), varargs) :: body)) ->
        makeVarargs varargs env parms body |> define env var
    | List (Atom "lambda" :: (List parms :: body)) -> makeNormalFunc env parms body
    | List (Atom "lambda" :: (DottedList(parms, varargs) :: body)) -> makeVarargs varargs env parms body
    | List (Atom "lambda" :: ((Atom _) as varargs :: body)) -> makeVarargs varargs env [] body
    | List (func :: args) ->
        let f = eval env func
        let argVals = List.map (eval env) args
        apply f argVals
    | badForm -> throw (BadSpecialForm("Unrecognized special form", badForm))
    and
        evalIf env pred conseq alt = 
            match eval env pred with 
            | Bool(false) -> eval env alt
            | _ -> eval env conseq
     

namespace Lisp

module Eval =
    open FParsec.CharParsers 

    open Ast
    open Errors
    open Parser
    open System.IO

    let rec last = function 
        | hd :: [] -> hd 
        | hd :: tl -> last tl 
        | _ -> failwith "Empty list."  


    let readOrThrow parser input =
        match run parser input with
            | Success(v, _, _) -> v
            | Failure (msg, err, _) -> raise (LispException(ParseError(msg, err)))
    
    let readExpr = readOrThrow parseExpr 
    let readExprList = readOrThrow (endBy parseExpr spaces) 


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

    let rec unpackStr = function
        | String s -> s
        | Number n -> n.ToString()
        | Bool b -> b.ToString()
        | List [s] -> unpackStr s
        | noString -> throw (TypeMismatch("string", noString))

    let rec unpackBool = function
        | Bool b -> b
        | List [b] -> unpackBool b
        | noBool -> throw (TypeMismatch("boolean", noBool))

    let tryUnpacker (unpack: LispVal -> 'a) (op: 'a -> 'a -> bool) arg1 arg2 =
        try op (unpack arg1) (unpack arg2) with _ -> false
    
    let numUnpackEq = tryUnpacker unpackNum (=)
    let strUnpackEq = tryUnpacker unpackStr (=)
    let boolUnpackEq = tryUnpacker unpackBool (=)
    
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

    let car = function
        | [List (x :: _)] -> x
        | [DottedList (x :: _, _)] -> x
        | [badArg] -> throw (TypeMismatch("pair", badArg))
        | badArgList -> throw (NumArgs(1, badArgList))

    let cdr = function
        | [List (x::xs)] -> List xs
        | [DottedList ([xs], x)] -> x
        | [DottedList ((_ :: xs), x)] -> DottedList (xs, x)
        | [badArg] -> throw (TypeMismatch("pair", badArg))
        | badArgList -> throw (NumArgs(1, badArgList))

    let cons = function
        | [x; List xs] -> List (x :: xs)
        | [x; DottedList (xs, xlast)] -> DottedList(x :: xs, xlast)
        | [x1; x2] -> DottedList([x1], x2)
        | badArgList -> throw (NumArgs(2, badArgList))
    
    let rec eqvPrim e1 e2 =
        match e1, e2 with
        | (Bool b1, Bool b2) -> b1 = b2
        | (Number n1, Number n2) -> n1 = n2
        | (String s1, String s2) -> s1 = s2
        | (Atom a1, Atom a2) -> a1 = a2
        | (DottedList (xs, x), DottedList(ys, y)) -> eqvPrim (List (xs @ [x])) (List (ys @ [y]))
        | (List l1, List l2) -> l1.Length = l2.Length && List.forall2 eqvPrim l1 l2
        | _ -> false

    let eqv = function
        | [e1; e2] -> Bool (eqvPrim e1 e2)
        | badArgList -> throw (NumArgs (2, badArgList))
    
    let equal = function
        | [arg1; arg2] ->
            let unpackEqual = numUnpackEq arg1 arg2 ||
                              strUnpackEq arg1 arg2 ||
                              boolUnpackEq arg1 arg1
            Bool (eqvPrim arg1 arg2 || unpackEqual)
        | argsList -> throw (NumArgs(2, argsList))


    let makePort fileAccess = fileIOFunction (fun fileName -> File.Open(fileName, FileMode.OpenOrCreate, fileAccess) |> Port)

    let closePort = function
        | [Port(port)] -> port.Close() ; Bool true
        | _ -> Bool false
    
    let rec readProc port = 
        let parseReader (reader: TextReader) = reader.ReadLine() |> readExpr
        match port with 
            | [] -> parseReader(System.Console.In)
            | [Port(port)] ->
                use reader = new StreamReader(port)
                parseReader (reader)
            | args -> throw (NumArgs(1, args))
    
    let writeProc objPort =
        let write obj (writer: TextWriter) = writer.Write(showVal obj) ; Bool true
        match objPort with 
            | [obj] -> write obj (System.Console.Out)
            | [obj ; Port(port)] ->
                use writer = new StreamWriter(port)
                write obj writer
            | args -> throw (NumArgs(1, args))
    
    let readContents = fileIOFunction (fun fileName -> File.ReadAllText(fileName) |> String)

    let readAll fileName = load fileName |> List

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
    and
        applyProc = function
            | [func; List args] -> apply func args
            | func :: args -> apply func args
            | [] -> throw (Default("Expecting a function, got an empty list"))


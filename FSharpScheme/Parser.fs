namespace Lisp

module Parser = 
    open FParsec.Primitives 
    open FParsec.CharParsers 

    open Ast
    
    type LispParser<'a> = Parser<'a, unit>
    type LispParser = Parser<LispVal, unit> 
   

    let spaces1: LispParser<unit> = skipMany1 spaces
    let endBy p sep = many (p .>> sep)
    let symbol : LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"
    let chr c = skipChar c

    let parseExpr, parseExprRef : LispParser * LispParser ref = createParserForwardedToRef()

    let parseAtom : LispParser = parse {
            let! first = letter <|> symbol
            let! rest = manyChars (letter <|> symbol <|> digit)
            return match first.ToString() + rest with
                   | "#t" -> Bool true
                   | "#f" -> Bool false
                   | atom -> Atom atom
    }

    let parseString : LispParser = parse {
            do! chr '"'
            let! xs = manyChars (noneOf "\"")
            do! chr '"'
            return String(xs)
    }

    let parseNumber : LispParser = many1Chars digit |>> (System.Int32.Parse >> Number)

    let parseQuoted : LispParser = chr '\'' >>. parseExpr |>> fun expr -> List [Atom "quote"; expr]

    let parseList : LispParser = sepBy parseExpr spaces1 |>> List

    let parseDottedList : LispParser = parse {
        let! head = endBy parseExpr spaces1
        let! tail = chr '.' >>. spaces1 >>. parseExpr
        return DottedList (head, tail)
    }

    do parseExprRef := parseAtom
                       <|> parseString
                       <|> parseNumber
                       <|> parseQuoted
                       <|> parse {
                                do! chr '('
                                let! x = (attempt parseList) <|> parseDottedList
                                do! chr ')'
                                return x
                          }
             
    let rec showVal = function 
        | String s -> "\"" + s + "\"" 
        | Atom a -> a 
        | Number n -> n.ToString() 
        | Bool b -> if b then "#t" else "#f" 
        | List l -> "(" + unwordsList l + ")" 
        | DottedList (head, tail) -> "(" + unwordsList head + " . " + showVal tail + ")" 
        | PrimitiveFunc(_) -> "<primitive>" 
        | Port (_) -> "<IO port>" 
        | Func({ parms = parms; varargs = varargs; body = body; closure = closure }) ->  
                                                "(lambda (" + unwordsList (parms |> List.map (String)) + 
                                                    (match varargs with 
                                                        | None -> "" 
                                                        | Some(arg) -> " . " + arg) + ") ...)" 
                                                         
    and 
        unwordsList = List.map showVal >> String.concat " " 





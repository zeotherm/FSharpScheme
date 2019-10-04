namespace Lisp

//module Parser = 
    //open FParsec.Primitives 
    //open FParsec.CharParsers 

    //open Ast
    
    //type LispParser<'a> = Parser<'a, unit>
    //type LispParser = Parser<LispVal, unit> 
   

    //let spaces1: LispParser<unit> = skipMany1 spaces
    //let endBy p sep = many (p .>> sep)
    //let symbol : LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"
    //let chr c = skipChar c

    //let parseExpr, parseExprRef : LispParser * LispParser ref = createParserForwardedToRef()

    //let parseAtom : LispParser = parse {
    //        let! first = letter <|> symbol
    //        let! rest = manyChars (letter <|> symbol <|> digit)
    //        return match first.ToString() + rest with
    //               | "#t" -> Bool true
    //               | "#f" -> Bool false
    //               | atom -> Atom atom
    //}

    //let parseString : LispParser = parse {
    //        do! chr '"'
    //        let! xs = manyChars (noneOf "\"")
    //        do! chr '"'
    //        return String(xs)
    //}

    //let parseStringX : LispParser = (chr '"' >>. manyChars (noneOf "\"") |>> String .>> chr '"') 

    //let parseNumber : LispParser = many1Chars digit |>> (System.Int32.Parse >> Number)

    //let parseNumberX : LispParser = pint32 |>> Number 

    //let parseQuoted : LispParser = chr '\'' >>. parseExpr |>> fun expr -> List [Atom "quote"; expr]

    //let parseList : LispParser = sepBy parseExpr spaces1 |>> List

    //let parseDottedList : LispParser = parse {
    //    let! head = endBy parseExpr spaces1
    //    let! tail = chr '.' >>. spaces1 >>. parseExpr
    //    return DottedList (head, tail)
    //}

    //do parseExprRef := parseAtom
    //                   <|> parseString
    //                   <|> parseNumber
    //                   <|> parseQuoted
    //                   <|> parse {
    //                            do! chr '('
    //                            let! x = (attempt parseList) <|> parseDottedList
    //                            do! chr ')'
    //                            return x
    //                      }
             
    //let rec showVal = function 
    //    | String s -> "\"" + s + "\"" 
    //    | Atom a -> a 
    //    | Number n -> n.ToString() 
    //    | Bool b -> if b then "#t" else "#f" 
    //    | List l -> "(" + unwordsList l + ")" 
    //    | DottedList (head, tail) -> "(" + unwordsList head + " . " + showVal tail + ")" 
    //    | PrimitiveFunc(_) -> "<primitive>" 
    //    | Port (_) -> "<IO port>" 
    //    | Func({ parms = parms; varargs = varargs; body = body; closure = closure }) ->  
    //                                            "(lambda (" + unwordsList (parms |> List.map (String)) + 
    //                                                (match varargs with 
    //                                                    | None -> "" 
    //                                                    | Some(arg) -> " . " + arg) + ") ...)" 
                                                         
    //and 
    //    unwordsList = List.map showVal >> String.concat " " 

module Parser = 
 
    open FParsec.Primitives 
    open FParsec.CharParsers 
    //open FParsec.OperatorPrecedenceParser 
 
    open Ast 
 
    type LispParser<'a> = Parser<'a, unit> 
    type LispParser = Parser<LispVal, unit> 
      
    // 8. utility functions 
    let spaces1 : LispParser<unit> = skipMany1 spaces 
    let chr c = skipChar c 
    let endBy  p sep = many  (p .>> sep) 
 
    let symbol : LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#" 
 
    // 4. computation, starts with ", but discards it, gets all the rest, err if \" 
    let parseString : LispParser = parse { 
        do! chr '"' 
        let! xs = manyChars (noneOf "\"") 
        do! chr '"' 
        return String(xs)         
    } 
 
    let parseStringX : LispParser = (chr '"' >>. manyChars (noneOf "\"") |>> String .>> chr '"') 
 
    // 3. Starts with a letter or symbol followed by many of letter/symbol/digit, special case for Bool 
    let parseAtom : LispParser = parse { 
            let! first = letter <|> symbol 
            let! rest = manyChars (letter <|> symbol <|> digit) 
            return match first.ToString() + rest with 
                   | "#t" -> Bool true 
                   | "#f" -> Bool false 
                   | atom -> Atom atom 
    } 
     
    // 4. 1 .. N digits, parsed and put into number, comment on function composition ... 
    let parseNumber : LispParser = many1Chars digit |>> (System.Int32.Parse >> Number) 
    let parseNumberX : LispParser = pint32 |>> Number 
 
    // 1. you need to refer to parseExpr from the productions below, hence the forward declaration trick 
    let parseExpr, parseExprRef : LispParser * LispParser ref = createParserForwardedToRef() 
 
    // 6. Just expressions separated by one or more spaces 
    let parseList : LispParser = sepBy parseExpr spaces1 |>> List 
 
    // 7. An expression, followed by spaces, a dot, spaces and an expression 
    let parseDottedList : LispParser = parse { 
        let! head = endBy parseExpr spaces1 
        let! tail = chr '.' >>. spaces1 >>. parseExpr 
        return DottedList (head, tail) 
    } 
 
    // 5. a quoted expression is just a \ followed by an expression and gets parsed to 'quote' 
    let parseQuoted : LispParser = chr '\'' >>. parseExpr |>> fun expr -> List [Atom "quote"; expr]  
 
    // 2. A LispVal can be one of the below, note that the definition for list need to backtrack to 
    // disambinguate the two cases 
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
 
    // 9. print formatted version of lispval  
    let rec showVal = function 
        | String contents -> "\"" + contents + "\"" 
        | Atom name -> name 
        | Number num -> num.ToString() 
        | Bool t -> if t then "#t" else "#f" 
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




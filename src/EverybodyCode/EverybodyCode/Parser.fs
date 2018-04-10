module Parser

    open AST
    open PositionTacking
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers
    open System

    let str_ws1 s = pstring s .>> spaces1
    let isLetterOrDigit c = isLetter c || isDigit c
    let notLetterOrDigit c = not(isLetter c && isDigit c)

    let pidentifier : Parser<string,unit> =
        let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '$'
        many1SatisfyL isIdentifierChar "identifier"

    let pTypeName : Parser<string,unit> =
        let p c = isLetter c || isDigit c || c = '_' || c = '.'
        many1SatisfyL p "type name"

    let pPropertyPath =
        sepBy pidentifier (pchar '.')
            |>> fun names -> System.String.Join(".", names |> Seq.toArray)
        
    let keyWordSet =
        System.Collections.Generic.HashSet<_>(
            [|"as"; "in"|]
        )
    let isKeyword = keyWordSet.Contains

    let pvarname = position .>>. pidentifier
    let public pvar = pidentifier |>> fun name -> Variable(name)

    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    let stringLiteral =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))

    let pCharLitteral =
      let normalChar = satisfy (fun c -> c <> '\\' && c <> '\'')
      between (pstring "'") (pstring "'")
              (normalChar <|> escapedChar)
              |>> (AST.value.Char >> Litteral)

    let pstringLiteral = stringLiteral |>> (AST.value.String >> Litteral)
    let ptrue = (stringReturn "true" (Bool true)) <|> (stringReturn "True" (Bool true))
    let pfalse = (stringReturn "false" (Bool false)) <|> (stringReturn "False" (Bool false))
    let pbool = (ptrue <|> pfalse) |>> fun v -> Litteral(v)

    let pdecimal = pfloat |>> AST.value.Decimal
    let pnumber = pdecimal |>> Litteral

    let (pCallExp: Parser<expression, unit>), pCallExpImpl = createParserForwardedToRef()
    
    let (expOrOpe: Parser<expression, unit>), expOrOpeImpl = createParserForwardedToRef()

    let (pApply: Parser<expression, unit>), pApplyImpl = createParserForwardedToRef()

    let ws = spaces
    
    let pexpr = 
        choice [
            pstringLiteral
            pCharLitteral
           // pCallExp //TODO uncomment when implemented
            pbool
            pnumber
            pvar            
        ]

    let pcomment : Parser<CodePosition * statement, unit> =
      position .>>. (pstring "//" >>. manyChars(satisfy (fun c -> c <> '\n' && c <> '\r')))
      |>> fun (pos,c) -> pos, Comment c

    let peol:Parser<unit, unit> = optional (spaces >>. ((pchar '\n') <|> (pchar '\r') <|> (pchar ';')) .>> spaces)
    let str_ws s = pstring s >>. ws

    let buildOperationParser () =
      let opp = new OperatorPrecedenceParser<expression,unit,unit>()
      let opexpr = opp.ExpressionParser
      let p2 = 
        //attempt pApply <|> //TODO uncomment when implemented
        (attempt pexpr .>> ws)

      let term = (attempt (between (str_ws "(") (str_ws ")") expOrOpe)) <|> p2
      opp.TermParser <- term

      opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Operation(x,Add,y)))
      opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Operation(x,Subtract,y)))
      opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Operation(x,Multiply,y)))
      opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Operation(x,Divide,y)))
      opp.AddOperator(InfixOperator("%", ws, 2, Associativity.Left, fun x y -> Operation(x,Modulo,y)))

      opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun x y -> Operation(x,Eq,y)))
      opp.AddOperator(InfixOperator("equals", ws, 1, Associativity.Left, fun x y -> Operation(x,Eq,y)))
      opp.AddOperator(InfixOperator("!=", ws, 1, Associativity.Left, fun x y -> Operation(x,Ne,y)))
      opp.AddOperator(InfixOperator("differs ", ws, 1, Associativity.Left, fun x y -> Operation(x,Ne,y)))

      opp.AddOperator(InfixOperator("<>", ws, 1, Associativity.Left, fun x y -> Operation(x,Ne,y)))
      opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, fun x y -> Operation(x,Lt,y)))
      opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, fun x y -> Operation(x,Gt,y)))
      opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, fun x y -> Operation(x,Le,y)))
      opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, fun x y -> Operation(x,Ge,y)))

      opp.AddOperator(PrefixOperator("-", ws, 1, true, fun x -> PrefixOp(Negative, x)))
      opp.AddOperator(PrefixOperator("++", ws, 1, true, fun x -> PrefixOp(IncBefore, x)))
      opp.AddOperator(PrefixOperator("--", ws, 1, true, fun x -> PrefixOp(DecBefore, x)))

      opp.AddOperator(PostfixOperator("++", ws, 1, true, fun x -> PostfixOp(IncAfter, x)))
      opp.AddOperator(PostfixOperator("--", ws, 1, true, fun x -> PostfixOp(DecAfter, x)))

      opp.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, fun x y -> Operation(x,Or,y)))
      opp.AddOperator(InfixOperator("&&", ws, 1, Associativity.Left, fun x y -> Operation(x,And,y)))
      opexpr

    let poperation = buildOperationParser ()

    let (pstatement: Parser<CodePosition * statement, unit>), pstatementimpl = createParserForwardedToRef()
    
    let pstatements = 
        many (ws >>. pstatement .>> ws)

// TODO 
    expOrOpeImpl 
        := poperation <|> pexpr
    
    pstatementimpl := pstring "todo" |>> fun _ -> raise(NotImplementedException "statements")

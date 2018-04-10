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
            pCallExp //TODO uncomment when implemented
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
        attempt pApply <|> //TODO uncomment when implemented
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

    let pApply' : Parser<expression, unit> =
        let p = sepBy (pexpr .>> ws) (ws .>> pchar '.' .>> ws)
        let expectedIdentifier = expected "Application list"
        fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status = Ok && reply.Result.Length > 1
            then 
                let exprs = reply.Result
                let e = List.head exprs
                let r = exprs |> List.skip 1
                let ast = r |> List.fold (fun acc i -> Apply(acc, i)) e
                Reply ast
            else
                stream.BacktrackTo(state)
                Reply(Error, expectedIdentifier)

    pApplyImpl := pApply'


    let mkpCall p =
        let pNoArgs = pchar '(' .>> ws .>> pchar ')' |>> fun _ -> []
        let noArgsCall = 
            p .>> ws .>>. pNoArgs
        let pArgs =
            between (pchar '(') (pchar ')')
                (sepBy expOrOpe (ws .>> pchar ',' .>> ws) )
        let withArgsCall = p .>> ws .>>. pArgs
        attempt withArgsCall <|> 
        attempt noArgsCall

    let pCall = mkpCall pidentifier

    pCallExpImpl :=
        pCall |>> fun (name, args) -> CallExp(name, args)

    let pInstanciate =
        str_ws "new" >>. (mkpCall pTypeName)
            |>> fun (name, args) ->
                    let args' =
                        args |> List.map(fun a -> None,a)
                    InstanciateType (name, args')

    let parrayItems = sepBy expOrOpe (pchar ',' .>> ws)
    let parray = between (str_ws "[") (ws .>> str_ws "]") parrayItems
                    |>> (AST.value.Array >> Litteral)

    let pArrayOffset =
        let p = pexpr <|> pApply <|> pCallExp
        p .>> ws .>> pchar '[' .>>. p .>> pchar ']'
        |>> ArrayOffset

    let pHashMap =
        let s = ws >>. pchar '{' >>. ws
        let e = ws >>. pchar '}' >>. ws
        let pItem =
            expOrOpe .>> ws .>> pchar ',' .>> ws .>>. expOrOpe .>> ws
        let pItems = sepBy pItem (pchar ';' .>> ws)
        between s e pItems
        |>> (HashMap >> Litteral)

    expOrOpeImpl 
        := poperation <|> pexpr
    pstatementimpl := pstring "todo" |>> fun _ -> raise(NotImplementedException "statements")

    expOrOpeImpl 
        := pInstanciate 
            <|> pHashMap
            <|> parray
            <|> attempt pArrayOffset
            <|> pCallExp
            <|> poperation
            <|> pexpr
            <|> attempt pApply  //TODO

    let pVarDeclare = 
        pstring "declare" >>. ws >>. (position .>>. pidentifier) .>> ws .>> pstring "=" .>> ws .>>. expOrOpe .>> ws
        |>> fun ((p,name),e) -> p,SetVar (true, name, AssignValue e)

    let pSetVar = 
        pstring "set" >>. ws >>. (position .>>. pidentifier) .>> ws .>> pstring "=" .>> ws .>>. expOrOpe .>> ws
        |>> fun ((p,name),e) -> p,SetVar (false, name, AssignValue e)

    let pAssignProperty =
        position .>>. pPropertyPath .>> ws .>> pstring "=" .>> ws .>>. expOrOpe
            |>> fun ((p,name),e) -> 
                p,SetVar (false, name, AssignValue e)

    let pinterval =
        pipe5 (str_ws "[")
                (pexpr <|> expOrOpe)
                (ws .>> str_ws "to" .>> ws)
                (pexpr <|> expOrOpe)
                (str_ws "]")
                (fun _ e1 _ e2 _ -> Litteral(Interval(e1, e2)))

    let pIfTerm = str_ws "if" >>. (position .>>. expOrOpe) .>> str_ws "then" .>> ws .>>. pstatements
    let pIf = 
        pIfTerm .>> ws .>> (str_ws "end")
        |>> fun ((p,cond), block) -> p, If(cond, block)

    let pIfElse = 
        pIfTerm .>> ws .>> (str_ws "else") .>>. pstatements .>> ws .>> str_ws "end"
        |>> fun (((p,cond), block), elseBlock) -> p, IfElse(cond, block, elseBlock)

    let pwhile =
        pipe2 (position .>>. (str_ws "while" >>. spaces >>. expOrOpe .>> spaces))
              (pstatements .>> str_ws "end")
              (fun (pos,e) block -> pos,While(e,block))

    let pForEach =
        position .>>.
            pipe3 (str_ws "foreach" >>. spaces >>. (position .>>. pvar) .>> spaces)
                  (str_ws "in" >>. (position .>>. pvar))
                  (pstatements .>> str_ws "end")
                  (fun (p1,item) (p2,items) block -> ForEach((p1,item),(p2,items), block))
  
    let pFuncImpl keyword =
        let pParams = 
            sepBy (ws >>. pidentifier .>> ws .>>. position .>>. pidentifier) (pchar ',')
            |>> List.map (fun ((s1 , p1) , s2) -> p1,(s1,s2))
        let pArgs = between (pchar '(') (pchar ')') pParams
        position .>>.
            pipe3
                (str_ws keyword >>. position .>>. pidentifier)
                (ws >>. pArgs)
                (ws >>. pstatements .>> str_ws "end")
                (fun pname args block -> (pname, args, block, None))
    let pFunc =
        pFuncImpl "function" |>> (fun (p,a) -> p,(Function a))

    let ptypemember = (pipe3 pvarname (anyOf ['!';' ';'\t';'\n';'\r']) (ws >>. pidentifier) (fun (pos,n) c t -> pos,TypeProperty(t,n, c = '!')))
    let pSimpleTypeDeclare =
          let ptypememberParams = str_ws "(" >>. many (ptypemember .>> ((pchar ';' >>. ws) <|> ws)) .>> str_ws ")"
          pipe2 (position .>>. (spaces >>. pstring "type" >>. spaces >>. pTypeName))
                (spaces >>. ptypememberParams)
                (fun (pos,n) p -> pos,DeclareType(n, None, skipPositions p))

    let pClass =
        let pMethod = pFuncImpl "method" |>> fun (p,f) -> p,(TypeMethod f)
        let pMethodList = many (attempt pMethod)
        pSimpleTypeDeclare .>> ws .>> str_ws "with" .>>. pMethodList .>> str_ws "end"
            |>> fun ((p1, DeclareType(name,basename,typeMembers)), methods) -> 
                    let members = typeMembers @ (methods |> List.map snd)
                    p1, DeclareType(name,basename,members)
    let pExternalType =
        pstring "@external" >>. ws >>. (pSimpleTypeDeclare <|> pClass)
            |>> fun (p, (DeclareType (a,b,c))) -> p,(ExternalType (a,b,c))

    type ForSettings =
        { Start:expression positioned
          End:expression positioned
          Step:expression positioned option
          VarialbeName:varname positioned }

    let pForSettingsNoStep =
            pipe3
                (str_ws "for" >>. position .>>. expOrOpe)
                (str_ws "to" >>. position .>>. expOrOpe)
                (ws .>> str_ws "as" >>. position .>>. pidentifier)
                (fun s e v -> 
                    { Start=s; End=e; Step=None; VarialbeName=v })
    let pForSettingsWithStep =
            pipe4
                (str_ws "for" >>. position .>>. expOrOpe)
                (str_ws "to" >>. position .>>. expOrOpe)
                (ws .>> str_ws "as" >>. position .>>. pidentifier)
                (ws >>. str_ws "by" >>. position .>>. expOrOpe)
                (fun s e v step -> 
                    { Start=s; End=e; Step=(Some step); VarialbeName=v })
    let pForSettings =
        position .>>.
            (
                attempt pForSettingsWithStep <|>
                attempt pForSettingsNoStep
            )

    let pFor =
        let pBody = ws >>. pstatements .>> str_ws "end"
        pipe2 pForSettings pBody 
            (fun (ps,s) block -> ps,For(s.Start, s.End, s.Step,s.VarialbeName,block))

    let pBreak = 
        position .>> str_ws "break"
            |>> fun p -> p,Break
    let pContinue = 
        position .>> str_ws "continue"
            |>> fun p -> p,Continue

    let pReturn =
        position .>> str_ws "return" .>>. expOrOpe
            |>> fun (p,e) -> p, Return e

    let pCallStatement =
        let pCallFunc = 
            position .>>. pCall
            |>> fun (p,(name,args)) -> p,CallStatement(None, name, args)
        
        let pCallMethod =
            position .>>. (pidentifier .>> pchar '.' .>>. pCall)
            |>> fun (p,(o,(name,args))) -> p,CallStatement((Some o), name, args)
        attempt pCallMethod <|> attempt pCallFunc

    let pApplyStatement =
        position .>>. pApply 
        |>> fun (p,Apply(a,b)) -> p,ApplyStatement(a,b)

    let pfluentInstanciate =
        let ps1 = pidentifier .>> spaces .>> str_ws "is a" .>>. pTypeName
        let rawP = expOrOpe
        let sp2 = pidentifier .>> spaces .>> str_ws "of" .>>. rawP
        let sp22 = sepBy sp2 (spaces .>> str_ws "and")
        let sp3 = ps1 .>> spaces .>> str_ws "with" .>>. sp22
        let psimple =
            ps1
            |>> fun (varname,typename) ->
                  SetVar(false, varname, AssignValue(InstanciateType(typename, [])))
        let pcomplex =
            sp3
            |>> fun ((varname,typename), parameters) ->
                  let ps = parameters |> List.map (fun (n,v) -> (Some n),v)
                  SetVar(false, varname, AssignValue(InstanciateType(typename, ps)))
        position .>>. (attempt pcomplex <|> attempt psimple)

    pstatementimpl :=
        attempt pVarDeclare <|>
        attempt pSetVar <|>
        attempt pAssignProperty <|>
        attempt pIf <|>
        attempt pIfElse <|>
        attempt pFor <|>
        attempt pForEach <|>
        attempt pContinue <|>
        attempt pBreak <|>
        attempt pReturn <|>
        attempt pwhile <|>
        attempt pFunc <|>
        attempt pClass <|>
        attempt pExternalType <|>
        attempt pApplyStatement <|>
        attempt pCallStatement <|>
        attempt pSimpleTypeDeclare <|>
        attempt pfluentInstanciate


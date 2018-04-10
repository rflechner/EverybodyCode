module JsCompiler

  open System.Text
  open FParsec
  open FParsec.Primitives
  open FParsec.CharParsers
  open AST
  open Parser
  open PositionTacking
  open System.Reflection
  
  type CompilationErrors = (CodePosition * string) list option

  let inspect (three:statement positioned list) : CompilationErrors =
    let countMandatory =
      List.sumBy(function | TypeProperty(_,_,true) -> 1 | _ -> 0)
    let listMandatory =
      List.filter(function | TypeProperty(_,_,true) -> true | _ -> false)

    let rec searchNotRecognized (tail:statement positioned list) (acc:(CodePosition*string) list) =
      match tail with
      | (po, NotRecognized line) :: t ->
        searchNotRecognized t ((po,line) :: acc)
      | _ :: t ->
        searchNotRecognized t acc
      | [] ->
        acc

    let rec searchTypeDeclarations (tail:statement positioned list) (acc:(string*int*CodePosition*(string option)*typeMember list) list) =
      match tail with
      | (po,ExternalType(name, baseType, p)) :: t
      | (po,DeclareType(name, baseType, p)) :: t -> //TODO: check constructor usage
        let mandatories = listMandatory p
        let c = name, mandatories.Length, po, baseType, mandatories
        searchTypeDeclarations t (c :: acc)
      | _ :: t ->
        searchTypeDeclarations t acc
      | [] -> acc
   
    let rec searchInstanciateTypes (tail:statement positioned list) (acc:(string*int*CodePosition) list) =
      match tail with
      | (po,SetVar (_,_, AssignValue(InstanciateType(name, p)))) :: t
      | (po,Return (InstanciateType(name, p))) :: t -> 
        let namedParams = 
            p |> List.filter(fst >> Option.isSome)
        let c = name, namedParams.Length, po
        searchInstanciateTypes t (c :: acc)
      | _ :: t ->
        searchInstanciateTypes t acc
      | [] -> acc

    let declarations = searchTypeDeclarations three []
    let instanciates = searchInstanciateTypes three []

    let childTypesErros = 
      declarations
      |> List.choose (function
           | name, _, po, (Some baseName), mandatories1 ->
              match declarations |> List.tryFind(fun (d,c, po, _, mandatories2) -> d = baseName) with
              | None -> Some (po, (sprintf "Cannot find %s" baseName))
              | Some (_,_, _, _, mandatories2) ->
                  let missingMandatories = mandatories2 |> List.except mandatories1
                  if List.isEmpty missingMandatories
                  then None
                  else 
                    let names = missingMandatories |> List.map (function | TypeProperty(n,_,_) -> n)
                    let message = sprintf "Missing base constructors params: %A" names
                    Some (po, message)
           | _ -> None
         )

    let errors =
      (instanciates 
        |> List.choose (
            function
            | (name, c, po) -> 
                match declarations |> List.tryFind(fun (d,c, po, _, _) -> d = name) with
                | None -> Some (po, (sprintf """Cannot instanciate unknown type "%s".""" name))
                | Some (_,cd,_,_,_) when cd >= c -> None
                | Some _ -> 
                    Some (po,(sprintf """Constructor "%s" is called with missing params.""" name))
            )
      ) @ 
      (searchNotRecognized three []) @ childTypesErros

    if errors.Length > 0
    then Some errors
    else None

  type CodePosition with
    member __.NewLine() =
      { __ with Line=__.Line+1; Column=0; }
    member __.AtColumn c =
      { __ with Column=c; }
    member __.ToColumn c =
      { __ with Column=(__.Column+c) }

  type CompiledPart = 
    { Generated:CodePosition
      Original:CodePosition
      Content:string }
  type OutputCode = 
    { Content:CompiledPart list ref
      CurrentPosition:CodePosition ref }
    static member Empty =
      { Content=ref []; CurrentPosition=ref CodePosition.Zero }
    member x.GetRawContent() =
      let parts = !x.Content |> List.sortBy (fun part -> part.Generated.Line, part.Generated.Column)
      let (b,_) =
        parts
        |> List.fold (
            fun ((sb:StringBuilder),line) part ->
              sb.Append part.Content |> ignore
              if line <> part.Generated.Line
              then sb.AppendLine() |> ignore
              sb,part.Generated.Line
            )
          (StringBuilder(), 0)
      b.ToString()
    member x.PushTo original (code:string) g coma =
      let tcode = code.Trim()
      let content = 
        if tcode.EndsWith(";") || tcode = ""
        then code
        elif coma then code + ";"
        else code
      x.CurrentPosition := g
      x.Content := !x.Content @ [{ Generated=g; Original=original; Content=content }]
      x
    member x.PushLineC original (c:ColumnNumber, code:string) =
      let g = (!x.CurrentPosition).NewLine().AtColumn c
      x.PushTo original code g true
    member x.PushLine original (code:string) =
      let g = (!x.CurrentPosition).NewLine()//.ToColumn(code.Length)
      x.PushTo original code g true
    member x.PushRawLine original (code:string) =
      let g = (!x.CurrentPosition).NewLine().ToColumn(code.Length)
      x.PushTo original code g false
    member x.Push original (code:string) =
      let g = (!x.CurrentPosition).ToColumn(code.Length)
      x.PushTo original code g true
    member x.PushRaw original (code:string) =
      let g = (!x.CurrentPosition).ToColumn(code.Length)
      x.PushTo original code g false

  let compileJs (three:statement positioned list) =
      
      let quote s = "\"" + s + "\""

      let translatePrefixOp op = 
          match op with 
          | Negative  -> "-"
          | IncBefore -> "++"
          | DecBefore -> "--"

      let translatePostfixOp op =
          match op with 
          | IncAfter -> "++"
          | DecAfter -> "--"

      let translateInfixOp op = 
          match op with
          | Add       -> "+"
          | Subtract  -> "-"
          | Multiply  -> "*"
          | Divide    -> "/"
          | Eq        -> "=="
          | Ne        -> "!="
          | Lt        -> "<"
          | Gt        -> ">"
          | Le        -> "<="
          | Ge        -> ">="
          | And       -> "&&"
          | Or        -> "||"
          | Modulo    -> "%"

      let translateExp : (expression -> string) ref = ref (fun _ -> "")

      let translateValue v = 
          match v with
          | Bool b    -> if b then "true" else "false"
          | Decimal d -> d.ToString(System.Globalization.CultureInfo.InvariantCulture)
          | String  s -> quote s
          | Array bl   -> 
              let builder = new StringBuilder("[")
              for i in [0..(bl.Length-1)] do
                  let b = bl.[i]
                  builder.Append(translateExp.Value(b)) |> ignore
                  if i <= bl.Length-2 then 
                      builder.Append(",") |> ignore
              builder.Append("]") |> ignore
              builder.ToString()
          | HashMap bl ->
              let builder = new StringBuilder("{")
              for i in [0..(bl.Length-1)] do
                  let (k,v) = bl.[i]
                  builder.Append(translateExp.Value(k)) |> ignore
                  builder.Append(":") |> ignore
                  builder.Append(translateExp.Value(v)) |> ignore
                  if i <= bl.Length-2 then 
                      builder.Append(",") |> ignore
              builder.Append("}") |> ignore
              builder.ToString()
          | Interval(Litteral(Decimal(start)), Litteral(Decimal(stop))) ->
              sprintf "__init_interval__(%f, %f)" start stop
          | Interval _ ->
              failwith "you can only use intervals with decimals for the moment"
          | _         -> failwithf "%s not implemented" (v.ToString())

      let compileFunctionCall (instance:string option, name:string, exps:expression list) =
          let builder = new StringBuilder()
          match instance with
          | Some i -> builder.Append (i + ".") |> ignore
          | None -> ()
          builder.Append name |> ignore
          builder.Append "(" |> ignore
          let len = exps.Length
          for i in [0..len-1] do
            let e = exps.Item i
            builder.Append(translateExp.Value e) |> ignore
            if i < len-1 then builder.Append(",") |> ignore
          builder.Append(")") |> ignore
          builder.ToString()

      let compileInstanciateType(name:string, exps:expression list) = 
        let builder = new StringBuilder(" new " + name + "(")
        let len = exps.Length
        for i in [0..len-1] do
          let e = exps.Item i
          builder.Append(translateExp.Value e) |> ignore
          if i < len-1 then builder.Append(",") |> ignore
        builder.Append(")") |> ignore
        builder.ToString()

      let rec translateExpImpl (s:expression) : string =
          match s with
          | Litteral v -> translateValue v
          | Variable v -> v
          | Constant v -> quote v
          | Operation (e1, o, e2) -> (translateExpImpl e1) + (translateInfixOp o) + (translateExpImpl e2)
          | PrefixOp (o, e) -> (translatePrefixOp o) + (translateExpImpl e)
          | PostfixOp (o, e) -> (translateExpImpl e) + (translatePostfixOp o)
          | CallExp  (name, exps) -> compileFunctionCall(None, name, exps)
          | Apply (exp1, exp2) -> 
                let js1 = translateExpImpl exp1
                let js2 = translateExpImpl exp2
                sprintf "%s.%s" js1 js2
          | InstanciateType(name, values) -> compileInstanciateType(name, values |> List.map(fun (_,i) -> i))
          | ArrayOffset(v, o) ->
              let vc = translateExpImpl v
              let oc = translateExpImpl o
              sprintf "%s[%s]" vc oc
          | _          -> failwithf "%s not implemented" (s.ToString())

      translateExp := translateExpImpl
      
      let rec loop (tail:statement positioned list) (output:OutputCode) : OutputCode =
          let unPositioned (s:'t) : 't positioned =
           CodePosition.Zero, s
          
          let (|UnPositioned|_|) s =
            Some (unPositioned s)

          let translateVarSet (p:CodePosition) (n:string) s firstAlloc =
            let code = 
              match s with 
              | AssignValue e when n.Contains(".") ->
                  sprintf "%s = %s" n (translateExpImpl e)
              | AssignValue e ->
                  sprintf "%s = %s" n (translateExpImpl e)
              | IncValue    e -> 
                  sprintf "%s += %s" n (translateExpImpl e)
              | DecValue    e -> 
                  sprintf "%s -= %s" n (translateExpImpl e)
            if firstAlloc
            then output.PushRaw CodePosition.Zero "var " |> ignore
            output.PushLine p code

          let translateForLoop (e1, e2, n, b) =
              output.PushRaw CodePosition.Zero "for(" |> ignore
              match e1 with
              | Litteral v -> 
                  (n + "=" + (translateValue v)) |> output.PushLine CodePosition.Zero |> ignore
              | Variable v -> 
                  (n + "=" + v) |> output.PushLine CodePosition.Zero |> ignore
              | _ -> (translateExpImpl e1) |> output.PushLine CodePosition.Zero |> ignore
              
              match e2 with
              | Litteral v -> 
                  (n + "<=" + (translateValue v)) |> output.PushLine CodePosition.Zero |> ignore
              | Variable v -> 
                  (n + "<=" + v) |> output.PushLine CodePosition.Zero |> ignore
              | _ -> (translateExpImpl e2) |> output.PushLine CodePosition.Zero |> ignore

              (n + "++){") |> output.PushRawLine CodePosition.Zero |> ignore
              let innerjs = loop b output
              "}" |> innerjs.PushRawLine CodePosition.Zero

          let translateForeachLoop ((p1,arr), (p2,v), b) =
              let arrjs = translateExpImpl arr
              let vjs = translateExpImpl v
              output.PushRaw CodePosition.Zero "for(" |> ignore
              output.PushRaw p1 ("var " + vjs + "_key in ") |> ignore
              output.PushRaw p2 arrjs |> ignore
              output.PushRaw CodePosition.Zero ("){") |> ignore
              output.PushLine CodePosition.Zero ("var " + vjs + "=" + arrjs + "[" + vjs + "_key];") |> ignore
              loop b output |> fun o -> o.PushRawLine CodePosition.Zero ("}")

          let translateAddEventHandler p (v,e,h) =
            sprintf """%s.addEventListener("%s",%s)""" v e h
              |> output.PushLine p

          let translateTypeMembers (members:typeMember list) =
            for m in members do
              match m with
              | TypeProperty (pname, ptype, pinconstructor) ->
                  let v = if pinconstructor then pname else "undefined"
                  sprintf "this.%s = %s;" pname v
                  |> output.PushRaw CodePosition.Zero
                  |> ignore
              | TypeMethod ((_,name), parameters, b, _) -> 
                  let builder = StringBuilder()
                  sprintf "this.%s = function(" name |> builder.Append |> ignore
                  let paramNames = parameters |> Seq.map(fun (_,(varname,_)) -> varname) |> Array.ofSeq
                  let rawParam = System.String.Join(",", paramNames)
                  builder.Append rawParam |> ignore
                  builder.Append ")" |> ignore
                  builder.Append "{" |> ignore
                  builder.ToString()
                  |> output.PushRaw CodePosition.Zero
                  |> ignore

                  let o2 = loop b output
                  o2.PushRaw CodePosition.Zero "};" |> ignore
            output.PushLine CodePosition.Zero "};"

          let translateDeclareType (name:string, members:typeMember list) =
            let constructorParams = 
              members
              |> List.choose (function | TypeProperty (n, _, c) when c -> Some n | _ -> None)
              |> List.toArray
              |> fun a -> System.String.Join(", ", a)
            sprintf "var %s = function(%s){" name constructorParams
            |> output.PushRaw CodePosition.Zero
            |> ignore
            translateTypeMembers members

          let translateDeclareChildType (name, baseName, members) =
            let constructorParams = 
              members
              |> List.choose (function | TypeProperty (n, _, c) when c -> Some n | _ -> None)
              |> List.toArray
              |> fun a -> System.String.Join(", ", a)
            sprintf "var %s = function(%s){" name constructorParams
            |> output.PushRaw CodePosition.Zero
            |> ignore
            sprintf "%s.call(this);" baseName
            |> output.PushRaw CodePosition.Zero
            |> ignore
            translateTypeMembers members

          match tail with
          | (p, SetVar (firstAlloc, n, s)) :: t -> 
              translateVarSet p n s firstAlloc
              |> loop t
          | (p, Comment c) :: t ->
              output.PushRawLine p ("//" + c) |> loop t
          | (p,If (e, b)) :: t -> 
              let jse = "if(" + (translateExpImpl e) + "){\n"
              output.PushRawLine p jse
                |> fun o -> 
                     loop b o
                |> fun o -> o.PushRawLine p "}"
                |> loop t
          | (p,IfElse (e, b1, b2)) :: t -> 
              let condition = "if(" + (translateExpImpl e) + ") {\n"
              output.PushRawLine p condition |> ignore
              loop b1 output |> ignore
              let ``else`` = "}else{\n"
              output.PushRawLine p ``else`` |> ignore
              loop b2 output |> ignore
              output.PushRawLine CodePosition.Zero "}"
              |> loop t
          | (p,While (e, b)) :: t ->
              let condition = "while(" + (translateExpImpl e) + "){\n"
              output.PushRawLine p condition |> ignore
              loop b output |> ignore
              output.PushRawLine CodePosition.Zero "}"
              |> loop t
          | (p,Break) :: t -> 
              output.PushRawLine CodePosition.Zero "break;"
              |> loop t
          | (p,Continue) :: t -> 
              output.PushRawLine CodePosition.Zero "continue;"
              |> loop t
          | (p,Function ((_,name),``params``,block,retExp)) :: t -> 
              output.PushRawLine p ("function " + name + "(") |> ignore
              let len = ``params``.Length
              for i in [0..len-1] do
                  let (p,(varname,typename)) = ``params``.Item i
                  output.PushRaw p varname |> ignore
                  if i < len-1 
                  then output.PushRaw CodePosition.Zero "," |> ignore
              output.PushRaw CodePosition.Zero "){"
              |> loop block
              |> fun o -> o.PushRaw CodePosition.Zero "}"
              |> loop t
          | (p,CallStatement (instance,name, exps)) :: t ->
              let js = compileFunctionCall(instance, name, exps)
              output.PushLine CodePosition.Zero js
              |> loop t
          | (p,For ((p1,e1), (p2,e2), step, (pv,n), b)) :: t ->
              translateForLoop (e1, e2, n, b) |> loop t
          | (p,ExternFunction _) :: t -> 
              loop t output
          | (p,ForEach (arr, v, b)) :: t ->
              translateForeachLoop (arr, v, b) |> loop t
          | (p,DeclareType(name, None, members)) :: t ->
              translateDeclareType (name, members) |> loop t
          | (p,DeclareType(name, Some baseName, members)) :: t ->
              translateDeclareChildType (name, baseName, members) |> loop t
          | (p,AddEventHandler(v,e,h)) :: t ->
              translateAddEventHandler p (v,e,h) |> loop t
          | (p, ExternalType _) :: t ->
              loop t output
          | (p, ApplyStatement(a,b)) :: t ->
              let js1 = translateExpImpl a
              let js2 = translateExpImpl b
              let line = sprintf "%s.%s" js1 js2
              output.PushLine p line
              |> loop t
          | (p, PatternMatching(target, conditions, defaultAction)) :: t ->
              let compiledConditions = 
                conditions 
                |> List.map (fun (cmp,condition,issue) -> cmp, translateExpImpl condition, translateExpImpl issue)

              let cases =
                compiledConditions
                |> List.mapi(fun i (cmp,condition,issue) -> 
                    let cif = if i <= 0 then "if" else "else if"
                    match cmp with
                    | MustEqual ->
                        cif + "(" + (translateExpImpl target) + " == " + condition + "){\n" + issue + "}\n"
                    | MustBeTrue ->
                        cif + "(" + condition + "){\n" + issue + "}\n"
                   )
              let lines = cases |> List.toArray
              let elsejs = "else {" + (translateExpImpl defaultAction) + "}"

              output.PushRawLine p (System.String.Join("\n", lines))
              |> fun o -> o.PushRawLine p elsejs
              |> loop t
          | (p, Return e) :: t ->
              output.PushRaw p "return ("
              |> fun o -> o.PushRaw p (translateExpImpl e)
              |> fun o -> o.PushRawLine p ");"
              |> loop t
          | []     -> output
          | c :: _ -> failwithf "cannot compile %s" (c.ToString())
      OutputCode.Empty |> loop three

  open System.IO

  let extractFramework outputPath =
    let asm = Assembly.GetExecutingAssembly()
    let name = "limpid_framework.js"
    use stream = asm.GetManifestResourceStream name
    use file = File.OpenWrite (Path.Combine(outputPath, "limpid_framework.js"))
    stream.CopyTo file
    stream.Flush()
    file.Flush()
    file.Close()


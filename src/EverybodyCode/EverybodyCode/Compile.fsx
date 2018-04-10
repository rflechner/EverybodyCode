#I @"../packages/Newtonsoft.Json/10.0.3/lib/net45"
#I "../packages/FParsec/1.0.4-rc/lib/net40-client/"

#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Newtonsoft.Json.dll"

#load "PositionTacking.fs"
#load "AST.fs"
#load "Parser.fs"
#load "JsCompiler.fs"

open AST
open PositionTacking
open JsCompiler
let compile e = 
    let code = compileJs [(CodePosition.Zero,e)]
    code.GetRawContent()

// declare a variable
let line1 = SetVar (true, "toto", AssignValue (Litteral (String "coucou")))

compile line1

// compile a simple if
compile <| If ((Operation (Litteral (Decimal 1.0),Eq,Variable "a")), [(CodePosition.Zero,line1)])

// compile a while loop if
compile <| While ((Operation (Litteral (Decimal 1.0),Eq,Variable "a")), [(CodePosition.Zero,line1)])

// generate a function call statement
let createEcho message = CallStatement(None, "echo", [(Litteral(String message))])

// compile a if / else
compile <| IfElse ((Operation (Litteral (Decimal 1.0),Eq,Variable "a")), [(CodePosition.Zero,(createEcho "in the if"))], [(CodePosition.Zero,(createEcho "in the else"))])


let props = 
    [ TypeProperty("Firstname", "string", true)
      TypeProperty("Lastname", "string", true)
      TypeProperty("Age", "int", true)
      TypeProperty("IsWoman", "bool", false) ]
let methods = 
    [ 
        TypeMethod (
            withZeroPosition "displayName", 
            [],
            [CallStatement(None, "echo", [Variable "this.name"]) |> withZeroPosition],
            None) ]

compile <| DeclareType("Customer", None, props @ methods)

let echoImpl =
    Function (
        withZeroPosition "echo",
        [withZeroPosition ("message","string")],
        [CallStatement(None, "console.log", [Variable "message"]) |> withZeroPosition],
        None)

compile echoImpl

let customerInstance =
    InstanciateType (
        "customer", 
        [
            Some "Firstname", Litteral(String "John")
            None, Litteral(String "Doe")
            None, Litteral(Decimal 33.)
        ]
    )
compile <| SetVar(true, "$c", AssignValue customerInstance)
compile <| SetVar(false, "$c", AssignValue customerInstance)


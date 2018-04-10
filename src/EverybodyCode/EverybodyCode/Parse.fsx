#I @"../packages/Newtonsoft.Json/10.0.3/lib/net45"
#I "../packages/FParsec/1.0.4-rc/lib/net40-client/"

#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Newtonsoft.Json.dll"

#load "PositionTacking.fs"
#load "AST.fs"
#load "Parser.fs"
#load "JsCompiler.fs"

open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open AST
open Parser
open PositionTacking

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test poperation "5 + 4 * (2+1)"

test poperation "1 == a"

// first example

let pVarDeclare1 = 
    pstring "declare" >>. ws >>. pidentifier .>> ws .>> pchar '=' .>> ws .>>. pexpr

let pVarDeclare = 
    pstring "declare" >>. ws >>. (position .>>. pidentifier) .>> ws .>> pstring "=" .>> ws .>>. pexpr
    |>> fun ((p,name),e) -> p,SetVar (true, name, AssignValue e)

test pVarDeclare "declare age = 33"
test pVarDeclare "declare age = age1"


module Tests

open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open AST
open Parser
open PositionTacking
open Xunit

let test parser str expected =
    match run parser str with
    | Success(result, _, _)   ->
        if result <> expected
        then 
            sprintf "%A is not equal to %A" result expected
                |> Exception |> raise
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let testWithoutPosition (parser:Parser<CodePosition*'t,_>) str (expected:'t) =
    match run parser str with
    | Success(result, _, _)   ->
        if snd result <> expected
        then 
            sprintf "%A is not equal to %A" result expected
                |> Exception |> raise
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg


[<Fact>]
let ``Parsing addition beetween a variable and a multiplication`` () =
    let exp = Operation (Litteral (Decimal 5.0),Add, Operation (Litteral (Decimal 4.0),Multiply, Operation (Litteral (Decimal 2.0),Add,Litteral (Decimal 1.0))))
    test poperation "5 + 4 * (2+1)" exp

// [<Fact>]
// let ``Parsing a variable declaration`` () =
//     let exp = SetVar (true,"age",AssignValue (Litteral (Decimal 33.)))
//     testWithoutPosition pVarDeclare "declare age = 33" exp

// [<Fact>]
// let ``Parsing condition`` () =
//     let exp = ({Line = 1; Column = 4;},
//                 If (Operation (Variable "toto",Eq,Litteral (Decimal 10.)), 
//                     [({Line = 1; Column = 32;}, SetVar (true,"age",AssignValue (Litteral (Decimal 33.))))]
//               ))
//     test pIf """if toto equals 10 then declare age = 33 end""" exp

// [<Fact>]
// let ``Parsing if else condition`` () =
//     let exp = ({Line = 1; Column = 4;},
//                 IfElse
//                  (Operation (Variable "toto",Eq,Litteral (Decimal 10.)),
//                   [({Line = 1;
//                      Column = 32;}, SetVar (true,"age",AssignValue (Litteral (Decimal 33.))))],
//                   [({Line = 1;
//                      Column = 54;}, SetVar (true,"age",AssignValue (Litteral (Decimal 40.))))]))
//     test pstatement "if toto equals 10 then declare age = 33 else declare age = 40 end" exp




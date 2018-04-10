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

// let pVarDeclare = 
//     pstring "declare" >>. ws >>. pidentifier .>> ws .>> pchar '=' .>> ws .>>. pexpr

let pVarDeclare = 
    pstring "declare" >>. ws >>. (position .>>. pidentifier) .>> ws .>> pstring "=" .>> ws .>>. pexpr
    |>> fun ((p,name),e) -> p,SetVar (true, name, AssignValue e)

test pVarDeclare "declare age = 33"
test pVarDeclare "declare age = age1"

//declare colors = [ "red", "green", "blue", "purple" ]

test pstatements """declare count = 0
declare colors = [ "red", "green", "blue", "purple" ]
console.log(colors)"""

test pApplyStatement """console.log(colors)"""

// test pSetVar "set age = age1"
// test pSetVar "set age = age + 1"
// test pIf "if toto equals 10 then declare age = 33 end"
// test pIfElse "if toto equals 10 then declare age = 33 else declare age = 40 end"

// test pIfElse 
//     """if toto equals 10 then 
//     declare age = 33 
//     declare name = "jean"
// else
//     declare age = 40
//     declare name = "marc" 
// end
// """

// test pstatements
//     """declare age1 = 40+1
// declare age2 = 30
// """

// test pstatement """declare age1 = 40+1"""
// test pstatements """declare age2 = 30 """
// test parray "[0, 64]"
// test parray "[0, 64, (1+4)]"
// test parray "[0, 64, ea]"
// test parray "[ea]"
// test pinterval "[0 to 64]"
// test pinterval "[0 to dsaz]"
// test pinterval "[0 to (45+4)]"

// test pwhile
//     """while i equals 5
//     declare age = 40
//     declare name = "marc" 
// end
// """

// test pForEach
//     """foreach item in items
//     declare age = 40
//     declare name = "marc" 
// end
// """
// test pReturn """return 1+popo """

// test pFunc """function displayMessage (string text, int size)
//     while i equals 5
//         declare age = 40
//         declare name = "marc" 
//     end
//     foreach item in items
//         set age = 21
//         set name = "jojo" 
//     end
// end
// """

// test pFunc """function displayMessage ()
//     return 2+9
// end
// """

// test pSimpleTypeDeclare """type Customer (string! name)"""
// test pSimpleTypeDeclare """type Customer (string name; int! age)"""

// test pClass
//     """type Customer (string! name) with
//     method displayMessage ()
//     end
    
//     method setAge (int years)
//         set age = age + years
//     end
// end"""

// test pFor """for 1 to 5 as i
//     foreach item in items
//         set age = 21
//         set name = "jojo" 
//     end
// end
// """

// test pstatement """for 1 to 5 as i by po
//     foreach item in items
//         set age = 21
//         break
//         set name = "jojo" 
//         continue
//     end
// end
// """

// test expOrOpe """myfunc (3, popo, lls)"""
// test expOrOpe """87 + myfunc(3) + 90"""
// test expOrOpe """myfunc ()"""

// test pCallStatement """myfunc (3, popo, lls)"""
// test pCallStatement """az.obj.myfunc (3, popo, lls)"""

// test pstatement """declare test = new Customer () """
// test pstatement """declare test = new Customer (po) """
// test pstatement """declare test = new Customer (98*l, toto) """

// test pstatement """test.Age = 33"""

// let plines = pstatements // .>> eof
// test plines """declare payButton = document.getElementById("payButton")"""

// test pCallExp """document.getElementById ("payButton") """

// let pCallChainExp =
//     sepBy pCallExp (ws .>> pchar '.' .>> ws)

// test pCallChainExp """toto () .tata()"""


// test plines """set lala = az.obj.myfunc (3, popo, lls)"""
// test expOrOpe """lala.toto ().titi"""
// test pstatements """set toto = popo.lzlzl.fr()"""
// test pstatements """declare payButton = document.getElementById ("payButton") """
// test pexpr """getElementById ("payButton") """

// let p = (sepBy (pexpr .>> ws) (ws .>> pchar '.' .>> ws)) .>> ws
// test p """document.getElementById ("payButton") """

// let pVarDeclare = 
//         pstring "declare" >>. ws >>. (position .>>. pidentifier) .>> ws .>> pstring "=" 
//             .>> ws .>>. expOrOpe .>> ws
//         |>> fun ((p,name),e) -> p,SetVar (true, name, AssignValue e)

// test pVarDeclare """declare payButton = document.getElementById ("payButton") """



// test pExternalType """@external type Game(Config config)"""

// open System.IO

// let (/>) a b = Path.Combine(a,b)
// let code = __SOURCE_DIRECTORY__ /> "game1" /> "source1.code" |> File.ReadAllText
// test pstatements code

// test pstatements """jQuery("#message").html("coucou number " + count + " ça marche !").hide().show()"""
// test pstatements """declare toto = []"""
// test expOrOpe "[0, 64]"
// test pstatement "colors.a()[count]"

// test pstatement """declare color = colors[count]"""
// test pstatement """jQuery("body").css("background-color", colors[count])"""

// test pstatements """function Coucou ()
  
//   if count < colors.length then
//     set count = count + 1
//   else
//     set count = 0
//   end

//   console.log("coucou number " + count)
//   jQuery("#message").html("coucou number " + count + " ça marche !")
//   jQuery("body").css("background-color", colors[count])
// end

// """

// test pApply "colors.length"
// test expOrOpe "i < colors.length"
// test pIf "if toto < colors.length then declare age = 33 end"

// test pfluentInstanciate "jean is a Customer"
// test pfluentInstanciate """jean is a Customer with Name of "Jean" """
// test pfluentInstanciate """jean is a Customer with Name of "Jean" and Age of 54 """

// test pstatement """declare config is a config with type of Phaser.AUTO """

// test pstatement """type Config (any type; int width; int height; any physics; any scene) """

// test pstatement """set a = { }"""
// test pstatement """set a = { 12, 84; "key1", 54 }"""

// //declare jean = new Customer ("toto", "tata", 54)
// //jean is a Customer with lastName of "toto" and firstName of "tata"

// let code' = """@external type sample.Customer (string! lastName; string! firstName; int age)
// jean is a sample.Customer with lastName of "toto" and firstName of "tata"
// jean = new sample.Customer ("toto", "tata", 54)
// """

// test pstatements code'

// open JsCompiler

// match run pstatements code' with
// | Success(result, _, _)   -> 
//     Some (inspect result)
// | Failure(errorMsg, _, _) -> 
//     printfn "Failure: %s" errorMsg
//     None

// //compileJs


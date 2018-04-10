module Compiler


open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open AST
open Parser
open PositionTacking
open JsCompiler
open SourceMapper
open System.IO

type ParsingResult =
| ParsingSuccess of statement positioned list
| ParsingError of string

let (/>) a b = Path.Combine(a,b)

let parseSourceCode (text:string) =
    match run pstatements text with
    | Success(result, _, _)   -> ParsingSuccess result
    | Failure(errorMsg, e, s) ->
        let s1 = s
        printfn "%A" s1
        ParsingError errorMsg

type MappedJavascript = 
    { SourceCode:string
      CodeMap:string }
    member __.GetRawContent filename =
        sprintf "%s\n//# sourceURL=%s\n//# sourceMappingURL=%s.map" 
            __.SourceCode filename filename
        
let compile script sourceName =
  async {
      match parseSourceCode script with
      | ParsingError e   -> 
            return Result.Error [sprintf "Error: %s" e]
      | ParsingSuccess three ->
          match inspect three with
          | None ->
            let code = compileJs three
            let js = code.GetRawContent()
            let! mapFile = code |> generateMapFile sourceName
            return Result.Ok { SourceCode=js; CodeMap=mapFile }
          | Some errors ->
            let lines = 
              errors 
              |> List.map (fun (p,error) -> sprintf "Error line %d column %d : %s" p.Line p.Column error)
            return Result.Error lines
  }

let compileToJs (sourcePath:string) outputFolder =
    let program = File.ReadAllText sourcePath
    let sourceName = Path.GetFileNameWithoutExtension sourcePath
    let sourceFile = Path.GetFileName sourcePath
    async {
        let! result = compile program sourceFile
        match result with
        | Result.Error errors -> 
            printfn "Compilation failed:"
            errors |> Seq.iter (printfn " - %s")
            return false
        | Result.Ok js ->
            File.WriteAllText(outputFolder /> sourceFile, program)
            File.WriteAllText(outputFolder /> (sourceName + ".js"), js.GetRawContent sourceFile)
            File.WriteAllText(outputFolder /> (sourceName + ".code.map"), js.CodeMap)
            printfn "Compilation succeed"
            return true
    }


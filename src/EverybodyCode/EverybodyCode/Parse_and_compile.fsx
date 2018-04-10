#I @"../packages/Newtonsoft.Json/10.0.3/lib/net45"
open System
#I "../packages/FParsec/1.0.4-rc/lib/net40-client/"

#r "FParsecCS.dll"
#r "FParsec.dll"
#r "Newtonsoft.Json.dll"

open System
open System.IO

#if x64
let env = "x64"
#else
let env = "x86"
#endif

let BuildFolder = Path.Combine (__SOURCE_DIRECTORY__, "BuildFolder")
Directory.CreateDirectory BuildFolder

#I "BuildFolder"

let copyFile dest src =
    File.Copy(src, dest, true)

let (/>) a b = Path.Combine(a,b)

let copyFiles (sourceFolder:string) (destFolder) (extensions:string list) =
    for ext in extensions do
        for f in Directory.EnumerateFiles(sourceFolder, "*" + ext) do
            let fn = Path.GetFileName f
            let dest = Path.Combine (destFolder, fn)
            f |> copyFile dest

let copyNodeDll env =
    let path = [| __SOURCE_DIRECTORY__ ; ".." ;"packages";"edge.js";"8.2.1";"content";"edge";env|]
    let folder = Path.Combine path
    let dest = Path.Combine(BuildFolder,"edge", env)
    Directory.CreateDirectory dest |> ignore
    copyFiles folder dest [".dll"; ".node"]

let copyEdgeJsFiles() =
    let path = [| __SOURCE_DIRECTORY__ ; ".." ;"packages";"edge.js";"8.2.1";"content";"edge"|]
    let folder = Path.Combine path
    let dest = Path.Combine(BuildFolder,"edge")
    Directory.CreateDirectory dest |> ignore
    copyFiles folder dest [".js"]


let copyEdgeDlls() =
    let path = [| __SOURCE_DIRECTORY__ ; ".." ;"packages";"edge.js";"8.2.1";"lib";"net40"|]
    let folder = Path.Combine path
    copyFiles folder BuildFolder [".dll"]

copyNodeDll "x86"
copyNodeDll "x64"
copyEdgeJsFiles()
copyEdgeDlls()

Environment.CurrentDirectory <- BuildFolder

#r "EdgeJs.dll"

#load "PositionTacking.fs"
#load "AST.fs"
#load "Parser.fs"
#load "JsCompiler.fs"
#load "SourceMapper.fs"
#load "Compiler.fs"

open Compiler

let compileAndRun () =
    async {
        let! success = compileToJs (__SOURCE_DIRECTORY__ /> "sample" /> "source1.code") BuildFolder
        if success
        then
            let htmlFile = BuildFolder /> "index.html"
            __SOURCE_DIRECTORY__ /> "sample" /> "framework.js" |> copyFile (BuildFolder /> "framework.js")
            __SOURCE_DIRECTORY__ /> "sample" /> "index.html" |> copyFile htmlFile
            System.Diagnostics.Process.Start("chrome", htmlFile) |> ignore
    } |> Async.RunSynchronously

//compileAndRun ()

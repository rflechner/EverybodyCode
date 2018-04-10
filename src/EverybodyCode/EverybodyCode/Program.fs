module Program

open System
open System.IO
open System.Diagnostics
open Compiler
open Argu

type Arguments =
    | [<Mandatory>] InputFolder of string
    | OutputFolder of string
    | AfterBuild of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | InputFolder _       -> "specify a folder containing files to compile."
      | OutputFolder _      -> "output folder."
      | AfterBuild _        -> "command to run after build succeed."
      
let cleanFolder folder =
    if Directory.Exists folder
    then
        Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories) |> Seq.iter File.Delete
        Directory.EnumerateDirectories(folder, "*", SearchOption.AllDirectories) |> Seq.iter Directory.Delete
    else
        folder |> Directory.CreateDirectory |> ignore

let rec directoryCopy srcPath dstPath copySubDirs =

    if not <| System.IO.Directory.Exists(srcPath) then
        let msg = System.String.Format("Source directory does not exist or could not be found: {0}", srcPath)
        raise (System.IO.DirectoryNotFoundException(msg))

    if not <| System.IO.Directory.Exists(dstPath) then
        System.IO.Directory.CreateDirectory(dstPath) |> ignore

    let srcDir = new System.IO.DirectoryInfo(srcPath)

    for file in srcDir.GetFiles() do
        let temppath = System.IO.Path.Combine(dstPath, file.Name)
        file.CopyTo(temppath, true) |> ignore

    if copySubDirs then
        for subdir in srcDir.GetDirectories() do
            let dstSubDir = System.IO.Path.Combine(dstPath, subdir.Name)
            directoryCopy subdir.FullName dstSubDir copySubDirs

[<EntryPoint>]
let Main argv =
    let parser = ArgumentParser.Create<Arguments>()
    let args = parser.Parse argv
    let inputFolder = args.GetResult(<@ InputFolder @>)
    let outputPath = args.GetResult(<@ OutputFolder @>, defaultValue=Environment.CurrentDirectory)
    let afterBuild = args.GetResult(<@ AfterBuild @>, defaultValue="")

    cleanFolder outputPath

    let buildResults =
        Directory.EnumerateFileSystemEntries(inputFolder, "*.code")
            |> Seq.fold (
                    fun acc i -> 
                        let r = compileToJs i outputPath |> Async.RunSynchronously
                        acc && r
                ) true
    if buildResults
    then
        directoryCopy inputFolder outputPath true
        // Directory.EnumerateFiles(inputFolder, "*", SearchOption.AllDirectories)
        // |> Seq.iter (fun i -> 
        //        let relativePath = i.Substring(inputFolder.Length)
        //        let fn = Path.GetFileName i
        //        let outputPath = outputPath /> relativePath
        //        let output = outputPath /> fn
        //        File.Copy(i, output, true))
        if afterBuild <> ""
        then
            Process.Start(Environment.CurrentDirectory /> afterBuild) |> ignore
        0
    else -1


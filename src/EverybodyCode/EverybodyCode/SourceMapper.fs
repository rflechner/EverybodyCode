module SourceMapper

open Newtonsoft.Json
open PositionTacking
open JsCompiler
open System
open System.Text
open System.IO
open System.Diagnostics

type SourceMapping =
  { [<JsonProperty("generated")>] Generated:CodePosition
    [<JsonProperty("original")>] Original:CodePosition
    [<JsonProperty("source")>] Source:string
    [<JsonProperty("name")>] Name:string }

let merge (strs:string list) =
    let a = strs |> List.toArray
    String.Join(String.Empty, a)

let toJsObject o =
  let serializer = JsonSerializer()
  use stringWriter = new StringWriter()
  use writer = new JsonTextWriter(stringWriter)
  writer.QuoteName <- false
  serializer.Serialize(writer, o)
  stringWriter.ToString()

let mappings sourceName (code:OutputCode) =
  !code.Content
    |> List.filter (fun p -> p.Original <> CodePosition.Zero)
    |> List.mapi (
        fun i part -> 
          let name = sprintf "mapping_%d" i
          { Generated=part.Generated; Original=part.Original; Source=sourceName; Name=name }
       )

let rawMappings sourceName code =
  mappings sourceName code
  |> List.map (toJsObject >> (sprintf "map.addMapping(%s);"))
  |> merge

let private generateJsMapper sourceName (code:OutputCode) =
  let start = """var sourceMap = require("source-map");
return function (data, callback) {
  var map = new sourceMap.SourceMapGenerator({
      file: "source1.js"
  });"""
  let sb = StringBuilder()
  sb.AppendLine start |> ignore
  rawMappings sourceName code
  |> sb.AppendLine
  |> ignore    
  sb.AppendLine "callback(null, map.toString());}" |> ignore
  sb.ToString()

let private generateNodeJsMapper sourceName (code:OutputCode) =
  let start = """var sourceMap = require("source-map");
  var map = new sourceMap.SourceMapGenerator({
      file: "source1.js"
  });"""
  let sb = StringBuilder()
  sb.AppendLine start |> ignore
  rawMappings sourceName code
  |> sb.AppendLine
  |> ignore    
  sb.AppendLine "console.log(map.toString());" |> ignore
  sb.AppendLine "setTimeout(function(){ process.exit();}, 1000).suppressOut;" |> ignore
  sb.ToString()

let runNodeScript (code:string) =
  let cleanLine (line:string) = 
    if not (String.IsNullOrEmpty line) && line.StartsWith "> " 
    then line.Substring 2
    else line
  let readOutput (output:StreamReader) =
    let rec loop acc =
        let line = output.ReadLine() |> cleanLine
        if String.IsNullOrEmpty line
        then acc
        else loop (line :: acc)
    loop [] |> List.skip 1 |> List.rev
  let p = ProcessStartInfo "node"
  p.WorkingDirectory <- System.AppDomain.CurrentDomain.BaseDirectory
  let pa = Path.Combine(p.WorkingDirectory, "sourcemapper.js")
  File.WriteAllText(pa, code)
  p.RedirectStandardInput <- true
  p.RedirectStandardOutput <- true
  p.RedirectStandardError <- true
  p.UseShellExecute <- false
  p.CreateNoWindow <- true
  p.Arguments <- pa
  let node = Process.Start p
  node.StandardOutput.ReadToEnd()

let generateMapFile sourceName (code:OutputCode) =
  async {
    // waiting for EdgeJs features on Mac OS
    if Environment.OSVersion.Platform = PlatformID.Unix
    then
      let mapper = generateNodeJsMapper sourceName code
      return runNodeScript mapper
    else
      let mapper = generateJsMapper sourceName code
      let f = EdgeJs.Edge.Func mapper
      let! r = f.Invoke(null) |> Async.AwaitTask
      return r.ToString()
  }


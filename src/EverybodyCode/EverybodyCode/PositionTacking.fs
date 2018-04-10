module PositionTacking

open FParsec
open Newtonsoft.Json

type LineNumber = int
type ColumnNumber = int

let atColumn n (c:'t) : ColumnNumber * 't =
  n, c

type CodePosition = 
  { [<JsonProperty("line")>] Line:LineNumber
    [<JsonProperty("column")>] Column:ColumnNumber }
  static member Of (p:Position) =
    {Line=int p.Line; Column=int p.Column}
  static member Zero 
    with get() =
      {Line=0; Column=0}
type positioned<'t> = CodePosition * 't

let skipPositions (l:'t positioned list) =
  l |> List.map (fun (_,i) -> i)

let position : Parser<_,_> = fun stream -> Reply (CodePosition.Of stream.Position)
let withPosition (p:Parser<_,_>) =
  (position .>>. p)

let withZeroPosition (l:'t) =
  CodePosition.Zero,l

let withZeroPositions (l:'t list) =
  l |> List.map withZeroPosition


namespace Aoc2022Lib

open System.IO

module ReadInput =
    let read path =
        path |> File.ReadLines |> Seq.toList
        
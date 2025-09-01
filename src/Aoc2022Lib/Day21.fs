/// <summary>
/// Advent of Code 2022 — Day 21
/// </summary>
module Aoc2022Lib.Day21

open System
open System.Collections.Generic
open System.Text.RegularExpressions

type ParseError = InvalidLine of string

let parseInput (lines: string list) =
    let monkeyDict = Dictionary<string, string>()
    let pattern = @"(^.{4})\:\s(.*$)"

    let rec parseLines (lines: string list) =
        match lines with
        | [] -> Ok monkeyDict
        | hd :: tl ->
            let m = Regex.Match(hd, pattern)

            if m.Success then
                monkeyDict.Add(m.Groups.[1].Value, m.Groups.[2].Value)
                parseLines tl
            else
                Error(InvalidLine hd)

    parseLines lines

let dfsP1 (inputDict: Dictionary<string, string>) =

    let rec loop (queue: string list) =
        match queue with
        | [] -> failwith "dfsP1: queue is empty (no node to evaluate)"
        | hd :: tl ->
            let cur = inputDict[hd]

            match cur with
            | s when Regex(@"^\d+$").IsMatch s -> int64 s
            | s when Regex(@"^(.*)([-+*\/])(.*)$").IsMatch s ->
                let m = Regex(@"^(.*)([-+*\/])(.*)$").Match s
                let op = m.Groups.[2].Value
                let lhs1 = m.Groups.[1].Value
                let lhs2 = m.Groups.[3].Value

                match op with
                | "+" -> loop (tl @ [ lhs1 ]) + loop (tl @ [ lhs2 ])
                | "-" -> loop (tl @ [ lhs1 ]) - loop (tl @ [ lhs2 ])
                | "*" -> loop (tl @ [ lhs1 ]) * loop (tl @ [ lhs2 ])
                | "/" -> loop (tl @ [ lhs1 ]) / loop (tl @ [ lhs2 ])
                | _ -> failwithf "dfsP1: unknown operator '%s'" s
            | _ -> failwithf "dfsP1: unrecognized expression for '%s'" hd

    loop [ "root" ]


let part1 (lines: string list) =
    let inputDict = parseInput lines

    match inputDict with
    | Ok d -> dfsP1 d |> string
    | Error e -> $"Error parsing input {e}"

let part2 (lines: string list) = "Not implemented"

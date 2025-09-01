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

let part1 (lines: string list) =
    let input = parseInput lines

    match input with
    | Ok -> "Not implemented"
    | Error e -> $"Error parsing input {e}"

let part2 (lines: string list) = "Not implemented"

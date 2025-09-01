/// <summary>
/// Advent of Code 2022 — Day 21
/// </summary>
module Aoc2022Lib.Day21

open System
open System.Collections.Generic
open System.Text.RegularExpressions

type ParseError = InvalidLine of string

[<Struct>]
type Monkey =
    | Root
    | Human

[<RequireQualifiedAccess>]
module Monkey =
    let toString =
        function
        | Monkey.Root -> "root"
        | Monkey.Human -> "humn"

let PATTERN = @"^(.*)([-+*\/])(.*)$"

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

let dfsP1 (inputDict: Dictionary<string, string>) (name: string) =

    let rec loop (stack: string list) (containsHumn: bool) =
        match stack with
        | [] -> failwith "dfsP1: stack is empty (no node to evaluate)"
        | hd :: tl ->
            let cur = inputDict[hd]

            match cur with
            | s when Regex(@"^\d+$").IsMatch s -> int64 s
            | s when Regex(PATTERN).IsMatch s ->
                let m = Regex(PATTERN).Match s
                let op = m.Groups.[2].Value
                let lhs1 = m.Groups.[1].Value.Trim()
                let lhs2 = m.Groups.[3].Value.Trim()
                let humn = Monkey.toString Monkey.Human
                let hasHumn = (lhs1 = humn) or (lhs2 = humn)

                match op with
                | "+" -> loop (tl @ [ lhs1 ]) hasHumn + loop (tl @ [ lhs2 ]) hasHumn
                | "-" -> loop (tl @ [ lhs1 ]) hasHumn - loop (tl @ [ lhs2 ]) hasHumn
                | "*" -> loop (tl @ [ lhs1 ]) hasHumn * loop (tl @ [ lhs2 ]) hasHumn
                | "/" -> loop (tl @ [ lhs1 ]) hasHumn / loop (tl @ [ lhs2 ]) hasHumn
                | _ -> failwithf "dfsP1: unknown operator '%s'" s
            | _ -> failwithf "dfsP1: unrecognized expression for '%s'" hd

    loop [ name ] false

let dependsOnHumn (inputDict: Dictionary<string, string>) (name: string) =

    let rec loop (stack: string list) =
        match stack with
        | [] -> false
        | hd :: _ when hd = (Monkey.toString Monkey.Human) -> true
        | hd :: tl ->
            let cur = inputDict[hd]

            match cur with
            | s when Regex(@"^\d+$").IsMatch s -> loop tl
            | s when Regex(PATTERN).IsMatch s ->
                let m = Regex(PATTERN).Match s
                let lhs1 = m.Groups.[1].Value.Trim()
                let lhs2 = m.Groups.[3].Value.Trim()

                let humn = Monkey.toString Monkey.Human

                if (lhs1 = humn) or (lhs2 = humn) then
                    true
                else
                    loop (tl @ [ lhs1; lhs2 ])

    loop [ name ]

let invertTarget (op: string) (known: int64) (knownIdx: int) (target: int64) : int64 =
    match op, knownIdx with
    | "+", _ -> target - known
    | "-", 1 -> target + known
    | "-", 0 -> known - target
    | "*", _ -> target / known
    | "/", 1 -> target * known
    | "/", 0 -> known / target
    | op, _ -> failwithf "Unknown operator: '%s'" op

let rec solveNext (inputDict: Dictionary<string, string>) (name: string) (target: int64) =

    let eqn = inputDict.[name]
    let m = Regex(PATTERN).Match eqn
    let sides = [ m.Groups.[1].Value.Trim(); m.Groups.[3].Value.Trim() ]
    let op = m.Groups.[2].Value.Trim()
    let mutable known: int64 = 0
    let mutable knownIdx = 0

    let idxH = sides |> List.findIndex (fun side -> dependsOnHumn inputDict side)
    let idxK = 1 - idxH

    known <- dfsP1 inputDict sides.[idxK]
    knownIdx <- idxK

    let newTarget = invertTarget op known knownIdx target

    match (sides.[idxH] = (Monkey.toString Monkey.Human)) with
    | true -> newTarget
    | false -> solveNext inputDict sides.[idxH] newTarget


let solvePart2 (inputDict: Dictionary<string, string>) =

    let rootEqn = inputDict[Monkey.toString Monkey.Root]
    let m = Regex(PATTERN).Match rootEqn
    let sides = [ m.Groups.[1].Value.Trim(); m.Groups.[3].Value.Trim() ]
    let mutable target = int64 0
    let mutable humnStart: string = String.Empty

    // pass 1 - figure out which side has humn and which is numeric
    let idxH = sides |> List.findIndex (fun side -> dependsOnHumn inputDict side)
    let idxK = 1 - idxH

    target <- dfsP1 inputDict sides.[idxK]
    humnStart <- sides.[idxH]

    solveNext inputDict humnStart target


let part1 (lines: string list) =
    let inputDict = parseInput lines

    match inputDict with
    | Ok d -> dfsP1 d (Monkey.toString Monkey.Root) |> string
    | Error e -> $"Error parsing input {e}"

let part2 (lines: string list) =
    let inputDict = parseInput lines

    match inputDict with
    | Ok d -> solvePart2 d |> string
    | Error e -> $"Error parsing input {e}"

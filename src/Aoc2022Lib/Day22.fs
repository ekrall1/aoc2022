/// <summary>
/// Advent of Code 2022 — Day 22
/// </summary>
module Aoc2022Lib.Day22

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

type GridArr = char array array

type Facing =
    | Right
    | Left
    | Up
    | Down

let initFacingDict<'V> (valueFor: string -> 'V) : Dictionary<string, 'V> =
    let d = Dictionary<string, 'V>()

    for uc in FSharpType.GetUnionCases typeof<Facing> do
        d.Add(uc.Name, valueFor uc.Name)

    d

let toFacingDictGrid (grid: GridArr) : Dictionary<int * int, Dictionary<string, int * int>> =
    let d = Dictionary()

    grid
    |> Array.mapi (fun r row -> row |> Array.mapi (fun c _ -> ((r, c), initFacingDict (fun _ -> (-1, -1)))))
    |> Array.concat
    |> Array.iter (fun (k, v) -> d.Add(k, v))

    d

let toGridArr (lines: string list) : GridArr =
    lines |> List.map (fun s -> s.ToCharArray()) |> List.toArray

let tokenizeInstr (s: string) : string list =
    Regex.Matches(s, @"\d+|[RL]")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList

let readInput (lines: string list) =
    let gridPattern = @"^\s*[\.\#]+\s*$"
    let rulesPattern = @"^[0-9+RL]+$"

    let (|Grid|Rules|Other|) (s: string) =
        match s with
        | x when Regex(gridPattern).IsMatch x -> Grid x
        | x when Regex(rulesPattern).IsMatch x -> Rules x
        | x -> Other x

    let rec matchLine (lines: string list) (grid: string list) (instructions: string) =
        match lines with
        | [] -> (List.rev grid) |> toGridArr, tokenizeInstr instructions
        | Grid g :: tl -> matchLine tl (g :: grid) instructions
        | Rules r :: tl -> matchLine tl grid r
        | _ :: tl -> matchLine tl grid instructions

    matchLine lines [] String.Empty

let isCell (ch: char) = ch = '.' || ch = '#'

let updateR
    (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>)
    (row: char array)
    (r: int)
    (c: int)
    (first: int)
    (last: int)
    =

    let inner = movementGrid.[(r, c)]

    let rightPos =
        match (c + 1) > last with
        | true -> if row.[first] = '#' then (r, c) else (r, first)
        | false -> if row.[c + 1] = '#' then (r, c) else (r, c + 1)

    inner.["Right"] <- rightPos

let updateL
    (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>)
    (row: char array)
    (r: int)
    (c: int)
    (first: int)
    (last: int)
    =

    let inner = movementGrid.[(r, c)]

    let leftPos =
        match (c - 1) < first with
        | true -> if row.[last] = '#' then (r, c) else (r, last)
        | false -> if row.[c - 1] = '#' then (r, c) else (r, c - 1)

    inner.["Left"] <- leftPos

let updateLR (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>) (grid: char array array) =

    let rows = grid.Length

    for r = 0 to rows - 1 do
        let row = grid.[r]
        let first = row |> Array.findIndex isCell
        let last = row |> Array.findIndexBack isCell
        let cols = row.Length

        for c = 0 to cols - 1 do
            updateR movementGrid row r c first last
            updateL movementGrid row r c first last

let updateD
    (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>)
    (col: char array)
    (r: int)
    (c: int)
    (first: int)
    (last: int)
    =

    let inner = movementGrid.[(r, c)]

    let downPos =
        match (r + 1) > last with
        | true -> if col.[first] = '#' then (r, c) else (first, c)
        | false -> if col.[r + 1] = '#' then (r, c) else (r + 1, c)

    inner.["Down"] <- downPos

let updateU
    (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>)
    (col: char array)
    (r: int)
    (c: int)
    (first: int)
    (last: int)
    =

    let inner = movementGrid.[(r, c)]

    let upPos =
        match (r - 1) < first with
        | true -> if col.[last] = '#' then (r, c) else (last, c)
        | false -> if col.[r - 1] = '#' then (r, c) else (r - 1, c)

    inner.["Up"] <- upPos

let updateUD (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>) (grid: char array array) =

    let rows = grid.Length
    let maxCols = grid |> Array.maxBy (fun r -> r.Length) |> (fun r -> r.Length)

    for c = 0 to maxCols - 1 do
        let col =
            Array.init rows (fun r -> if c < grid.[r].Length then grid.[r].[c] else ' ')

        if Array.exists isCell col then
            let first = col |> Array.findIndex isCell
            let last = col |> Array.findIndexBack isCell

            for r = 0 to rows - 1 do
                if c < grid.[r].Length && isCell grid.[r].[c] then
                    updateD movementGrid col r c first last
                    updateU movementGrid col r c first last

let rotateRight (dr, dc) = (dc, -dr) // CW
let rotateLeft (dr, dc) = (-dc, dr) // CCW

let tryMove (pos: int * int) (facing: int * int) (movementGrid: Dictionary<int * int, Dictionary<string, int * int>>) =
    match facing with
    | (0, 1) -> movementGrid.[pos].["Right"]
    | (0, -1) -> movementGrid.[pos].["Left"]
    | (1, 0) -> movementGrid.[pos].["Down"]
    | (-1, 0) -> movementGrid.[pos].["Up"]
    | _ -> failwith "invalid facing direction"

let scoreFacing (facing: int * int) =
    match facing with
    | (0, 1) -> 0
    | (1, 0) -> 1
    | (0, -1) -> 2
    | (-1, 0) -> 3
    | _ -> failwith "invalid facing direction"

let part1 (lines: string list) =
    let grid, instructions = readInput lines
    let movementGrid = toFacingDictGrid grid
    updateLR movementGrid grid
    updateUD movementGrid grid

    let startingCol = grid.[0] |> Array.findIndex (fun ch -> ch = '.')
    let mutable pos = (0, startingCol)
    let mutable facing = (0, 1) // right

    instructions
    |> List.iter (function
        | "R" -> facing <- rotateRight facing
        | "L" -> facing <- rotateLeft facing
        | n ->
            let moveSpaces = int n

            Seq.replicate moveSpaces ()
            |> Seq.iter (fun _ -> pos <- tryMove pos facing movementGrid))

    (fst pos + 1) * 1000 + (snd pos + 1) * 4 + scoreFacing facing |> string

let part2 (lines: string list) = "Not implemented"

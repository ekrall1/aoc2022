/// <summary>
/// Advent of Code 2022 — Day 7
/// </summary>
module Aoc2022Lib.Day17

open System.Text.RegularExpressions
open Aoc2022Lib.Railway
open System.Collections.Generic
open System

type Row = Row of int

type Rows = Row array

type Shapes = Rows array

type Moves = string

module Row =
    let create (n: int) =
        if n < 0 || n > 0b1111111 then // 7-bit
            failwithf "Row value out of range: %d" n
        Row n

    let value (Row n) = n

    let inline band (Row a) (Row b) = Row (a &&& b)
    
    let inline bor (Row a) (Row b) = Row (a ||| b)

    let inline shl (Row a) k = create ((a <<< k) &&& 0b1111111)  // 7-bit

    let inline shr (Row a) k = create (a >>> k)


let ShapesRaw = "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"

let private toBits7 (n:int) =
    Convert.ToString(n, 2).PadLeft(7, '0')  // 7-bit view

let dumpShapes (shapes: Shapes) =
    shapes
    |> Array.iteri (fun i rows ->
        printfn "Shape %d:" i
        rows
        |> Array.iter (fun r ->
            let n = Row.value r
            printfn "  %3d  %s" n (toBits7 n))
        printfn "")

let buildShapeArray (raw: string): Shapes =
    let norm = raw.Replace("\r\n", "\n").Trim()
    let shapes = norm.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)

    let toRow (line: string) =
        let chars = line.Trim().ToCharArray()
        chars
        |> Array.mapi (fun i c ->
            if c = '#' then 1 <<< (6 - i) else 0)
        |> Array.sum
        |> fun x -> x >>> 2
        |> Row.create

    shapes
    |> Array.map (fun shape -> 
        shape.Split('\n', System.StringSplitOptions.RemoveEmptyEntries ||| System.StringSplitOptions.TrimEntries)
        |> Array.map toRow)
   

let part1 (lines: string list) =
    let shapes = buildShapeArray ShapesRaw
    dumpShapes shapes
    let well = Array.init 100 (fun _ -> Row.create 0b1000001)
    well.[0] <- Row.create 0b1111111
    "ok"

let part2 (lines: string list) = "not implemented"
/// <summary>
/// Advent of Code 2022 — Day 7
/// </summary>
module Aoc2022Lib.Day17

open System.Text.RegularExpressions
open Aoc2022Lib.Railway
open System.Collections.Generic

type Row = private Row of int

type Rows = Row array

module Row =
    let create (n: int) =
        if n < 0 || n > 0b1111111 then // 7-bit
            failwithf "Row value out of range: %d" n
        Row n

    let value (Row n) = n

    let inline band (Row a) (Row b) = Row (a &&& b)
    
    let inline bor (Row a) (Row b) = Row (a ||| b)

    let inline shl (Row a) k = create ((a <<< k) &&& 0b1111111))  // 7-bit

    let inline shr (Row a) k = create (a >>> k)

let part1 (lines: string list) = "not implemented"

let part2 (lines: string list) = "not implemented"
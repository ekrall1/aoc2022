namespace Aoc2022Lib

open System
open System.Text.RegularExpressions


module Day11 =

    type Operation =
        | Add of int
        | Multiply of int
        | Square

    type Monkey =
        { Items: int list
          Operation: Operation
          Divisor: int
          TrueMonkey: int
          FalseMonkey: int }

    let part1 (lines: string list) : string = "Not implemented"

    let part2 (lines: string list) : string = "Not implemented"

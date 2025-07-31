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

    let parseOperation (line: string) : Operation =
        let m = Regex.Match(line.Trim(), "new = old ([*+]) (.+)")

        if not m.Success then
            failwithf "Invalid operation line: %s" line
        else
            let op = m.Groups.[1].Value
            let rhs = m.Groups.[2].Value

            match op, rhs with
            | "*", "old" -> Square
            | "*", n -> Multiply(int n)
            | "+", n -> Add(int n)
            | _ -> failwithf "Unexpected operation: %s" line

    let parseItems (line: string) : int list =
        line.Substring(line.IndexOf(':') + 1)
        |> fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim() |> int)
        |> Array.toList

    let parseTest (line: string) : int =
        line.Trim().Split(' ') |> Array.last |> int

    let parseMonkey (lines: string list) : Monkey =
        match lines with
        | _name :: items :: op :: test :: ifTrue :: ifFalse :: [] ->
            { Items = parseItems items
              Operation = parseOperation op
              Divisor = parseTest test
              TrueMonkey = parseTest ifTrue
              FalseMonkey = parseTest ifFalse }
        | _ -> failwithf "Invalid monkey block: %A" lines

    let rec splitGroups (lines: string list) (current: string list) (acc: string list list) : string list list =
        match lines with
        | [] ->
            if current = [] then
                List.rev acc
            else
                List.rev ((List.rev current) :: acc)
        | "" :: tl -> splitGroups tl [] ((List.rev current) :: acc)
        | hd :: tl -> splitGroups tl (hd :: current) acc

    let parseInput (lines: string list) : Monkey list =
        lines |> (fun x -> splitGroups x [] []) |> List.map parseMonkey

    let part1 (lines: string list) : string = "Not implemented"

    let part2 (lines: string list) : string = "Not implemented"

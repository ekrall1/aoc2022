namespace Aoc2022Lib

open System
open System.Text.RegularExpressions


module Day11 =

    type Operation =
        | Add of int64
        | Multiply of int64
        | Square

    type Monkey =
        { Items: int64 list
          Operation: Operation
          Divisor: int64
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
            | "*", n -> Multiply(int64 n)
            | "+", n -> Add(int64 n)
            | _ -> failwithf "Unexpected operation: %s" line

    let parseItems (line: string) : int64 list =
        line.Substring(line.IndexOf(':') + 1)
        |> fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim() |> int64)
        |> Array.toList

    let parseTest (line: string) : int64 =
        line.Trim().Split(' ') |> Array.last |> int64

    let parseTestInt (line: string) : int =
        line.Trim().Split(' ') |> Array.last |> int

    let parseMonkey (lines: string list) : Monkey =
        match lines with
        | _name :: items :: op :: test :: ifTrue :: ifFalse :: [] ->
            { Items = parseItems items
              Operation = parseOperation op
              Divisor = parseTest test
              TrueMonkey = parseTestInt ifTrue
              FalseMonkey = parseTestInt ifFalse }
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

    let applyOperation (op: Operation) (value: int64) =
        match op with
        | Add n -> value + n
        | Multiply n -> value * n
        | Square -> value * value

    let simulate (rounds: int) (monkeys: Monkey array) : int64 array =
        let inspection: int64 array = Array.zeroCreate monkeys.Length

        for _ in 1..rounds do
            for i = 0 to monkeys.Length - 1 do
                let m: Monkey = monkeys.[i]
                inspection.[i] <- inspection.[i] + int64 (List.length m.Items)
                monkeys.[i] <- { m with Items = [] }

                for item: int64 in m.Items do
                    let mutable worry: int64 = applyOperation m.Operation item
                    worry <- worry / 3L

                    let dest: int =
                        if worry % m.Divisor = 0L then
                            m.TrueMonkey
                        else
                            m.FalseMonkey

                    let target: Monkey = monkeys.[dest]

                    monkeys.[dest] <-
                        { target with
                            Items = target.Items @ [ worry ] }

        inspection

    let part1 (lines: string list) : string =
        let monkeys = parseInput lines |> Array.ofList
        let inpsections = simulate 20 monkeys

        inpsections
        |> Array.sortDescending
        |> fun arr -> (arr.[0] * arr.[1]).ToString()

    let part2 (lines: string list) : string = "Not implemented"

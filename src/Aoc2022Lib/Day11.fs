namespace Aoc2022Lib

open System
open System.Text.RegularExpressions
open Utils


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

    let parseInput (lines: string list) : Result<Monkey list, string> =
        try
            lines |> (fun x -> splitGroups x [] []) |> List.map parseMonkey |> Ok
        with ex ->
            Error $"parseInput failed: {ex.Message}"

    let applyOperation (op: Operation) (value: int64) =
        match op with
        | Add n -> value + n
        | Multiply n -> value * n
        | Square -> value * value

    let runRounds rounds (adjust: int64 -> int64) (monkeys: Monkey list) =
        let ms = monkeys |> List.toArray

        let queues =
            ms |> Array.map (fun m -> System.Collections.Generic.Queue<int64>(m.Items))

        let inspections = Array.zeroCreate<int64> ms.Length

        for _ in 1..rounds do
            for i = 0 to ms.Length - 1 do
                let q = queues.[i]

                while q.Count > 0 do
                    let item = q.Dequeue()
                    inspections.[i] <- inspections.[i] + 1L
                    let worry = item |> applyOperation ms.[i].Operation |> adjust

                    let dest =
                        if worry % ms.[i].Divisor = 0L then
                            ms.[i].TrueMonkey
                        else
                            ms.[i].FalseMonkey

                    queues.[dest].Enqueue(worry)

        inspections


    let part1 (lines: string list) : string =
        parseInput lines
        |> Result.map (fun monkeys ->
            let inspections = runRounds 20 (fun x -> x / 3L) monkeys
            inspections |> Array.sortDescending |> (fun arr -> arr.[0] * arr.[1]) |> string)
        |> function
            | Ok result -> result
            | Error msg -> $"Error: {msg}"

    let part2 (lines: string list) : string =
        parseInput lines
        |> Result.map (fun monkeys ->
            let lcmAll = monkeys |> List.map (fun m -> m.Divisor) |> List.reduce lcm
            let inspections = runRounds 10000 (fun x -> x % lcmAll) monkeys
            inspections |> Array.sortDescending |> (fun arr -> arr.[0] * arr.[1]) |> string)
        |> function
            | Ok result -> result
            | Error msg -> $"Error: {msg}"

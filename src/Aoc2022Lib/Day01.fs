namespace Aoc2022Lib

open Aoc2022Lib.Utils

module Day01 =

    let GetMaxCalories (input: list<string>) : Result<int, string> =
        let rec FindMax (lst: list<string>) (acc: int) (most: int) : Result<int, string> =
            match lst with
            | [] -> max acc most |> Ok
            | hd :: tl ->
                if hd = "" then
                    FindMax tl 0 (max acc most)
                else
                    parseInt hd |> Result.bind (fun n -> FindMax tl (acc + n) most)

        FindMax input 0 0

    let GetListOfSums (input: list<string>) : Result<list<int>, string> =
        let rec FlattenSums (lst: list<string>) (acc: list<int>) (cur: int) : Result<list<int>, string> =
            match lst with
            | [] -> cur :: acc |> List.rev |> Ok
            | hd :: tl ->
                if hd = "" then
                    FlattenSums tl (cur :: acc) 0
                else
                    parseInt hd |> Result.bind (fun n -> FlattenSums tl acc (cur + n))

        FlattenSums input [] 0

    let GetTopNSum (lst: list<int>) (n: int) : Result<int, string> =
        if List.length lst < n then
            Error $"Not enough data to take top {n} elements"
        else
            lst |> List.sortDescending |> List.take n |> List.sum |> Ok


    let part1 input =
        input
        |> GetMaxCalories
        |> Result.map string
        |> Result.defaultValue "Error calculating max calories"

    let part2 input =
        input
        |> GetListOfSums
        |> Result.bind (fun lst -> GetTopNSum lst 3)
        |> Result.map string
        |> Result.defaultValue "Error adding up top three values"

namespace Aoc2022Lib

module Day01 =

    let parseInt (s: string) : Result<int, string> =
        match System.Int32.TryParse s with
        | true, n -> Ok n
        | false, _ -> Error $"Invalid integer: '{s}'"

    let GetMaxCalories (input: list<string>) : Result<int, string> =
        let rec FindMax (lst: list<string>) (acc: int) (most: int) : Result<int, string> =
            match lst with
            | [] -> Ok (max acc most)
            | hd :: tl ->
                if hd = "" then
                    FindMax tl 0 (max acc most)
                else
                    match parseInt hd with
                    | Error e -> Error e
                    | Ok n -> FindMax tl (acc + n) most

        FindMax input 0 0

    let GetListOfSums (input: list<string>) : Result<list<int>, string> =
        let rec FlattenSums (lst: list<string>) (acc: list<int>) (cur: int) : Result<list<int>, string> =
            match lst with
            | [] -> Ok (cur :: acc |> List.rev)
            | hd :: tl ->
                if hd = "" then
                    FlattenSums tl (cur :: acc) 0
                else
                    parseInt hd
                    |> Result.bind (fun n -> FlattenSums tl acc (cur + n))

        FlattenSums input [] 0

    let GetTopNSum (lst: list<int>) (n: int) : Result<int, string> =
        if List.length lst < n then
            Error $"Not enough data to take top {n} elements"
        else
            lst
            |> List.sortDescending
            |> List.take n
            |> List.sum
            |> Ok


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

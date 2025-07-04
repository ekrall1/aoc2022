namespace Aoc2022Lib

module Day06 =
(** Part 1 - identify the first position where the four most recently received characters were all different *)

    let FindMarker (line: string) : Result<int, string> =

        let rec check (str: string) (pos : int) : Result<int, string> =
            if pos > String.length str then
                Error "Unable to find marker position"
            else
                let substrSet = str.Substring(pos, 4).ToCharArray() |> Set.ofArray
                match Set.count substrSet with
                | 4 -> Ok (pos + 4)
                | _ -> check str (pos + 1)

        check line 0

    let SumFirstPositions (input: list<string>) : Result<int, string> =
        input |> List.fold (fun acc line -> 
            match acc, FindMarker line with
            | Ok runningTotal, Ok n -> Ok (runningTotal + n)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        ) (Ok 0)

    let part1 input =
        match SumFirstPositions input with
        | Ok finalSum -> string finalSum
        | Error msg -> $"Error summing positions for part 1: {msg}"

    let part2 input =
        "not implemented"

namespace Aoc2022Lib

module Day06 =
(** Part 1 - identify the first position where the four most recently received characters were all different *)
(** Part 2 - identify the first position where the fourteen most recently received characters were all different *)

    let FindMarker (line: string) (targetLen: int): Result<int, string> =

        let rec check (str: string) (pos : int) : Result<int, string> =
            if pos > String.length str then
                Error "Unable to find marker position"
            else
                let substrSet = str.Substring(pos, targetLen).ToCharArray() |> Set.ofArray
                match Set.count substrSet = targetLen with
                | true -> Ok (pos + targetLen)
                | _ -> check str (pos + 1)

        check line 0

    let SumFirstPositions (input: list<string>) (targetLen: int): Result<int, string> =
        input |> List.fold (fun acc line -> 
            match acc, FindMarker line targetLen with
            | Ok runningTotal, Ok n -> Ok (runningTotal + n)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        ) (Ok 0)

    let part1 input =
        match SumFirstPositions input 4 with
        | Ok finalSum -> string finalSum
        | Error msg -> $"Error summing positions for part 1: {msg}"

    let part2 input =
        match SumFirstPositions input 14 with
        | Ok finalSum -> string finalSum
        | Error msg -> $"Error summing positions for part 1: {msg}"

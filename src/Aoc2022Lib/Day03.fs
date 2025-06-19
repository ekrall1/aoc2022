namespace Aoc2022Lib

module Day03 =

    let SplitInHalf (s: string) : Result<string * string, string> =
        if s.Length % 2 <> 0 then
            Error $"Input string must have an even number of characters: {s}"
        else
            let half = s.Length / 2
            Ok (s.Substring(0, half), s.Substring(half))

    let FindCommon (s1: string) (s2: string) : Result<char, string> =
        let rec Search (str1: string) =
            match str1 with
            | "" -> Error "No common characters found"
            | _ ->
                let first = str1.[0]
                let rest = str1.Substring(1)
                if s2.Contains(first) then Ok first else Search rest

        Search s1

    let ScoreChar (c: char) : Result<int, string> =
        match c with
        | c when 'a' <= c && c <= 'z' -> Ok (int c - int 'a' + 1)
        | c when 'A' <= c && c <= 'Z' -> Ok (int c - int 'A' + 27)
        | _ -> Error $"Invalid character for scoring: {c}"

    let ScoreLine (line: string) : Result<int, string> =
        SplitInHalf line
        |> Result.bind (fun (s1, s2) -> FindCommon s1 s2)
        |> Result.bind ScoreChar

    let ScoreAllRows (input: list<string>) : Result<int, string> =
        input
        |> List.map ScoreLine
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok sum, Ok score -> Ok (sum + score)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        ) (Ok 0)


    let GetGroupsOfThree (input: list<string>) : Result<list<string * string * string>, string> =
        let rec loop lst acc =
            match lst with
            | a :: b :: c :: rest -> loop rest ((a, b, c) :: acc)
            | [] -> Ok (List.rev acc)
            | _ -> Error "Input does not contain a multiple of 3 lines"
        loop input []

    let FindCommonInGroupsOfThree (s1: string) (s2: string) (s3: string) : Result<char, string> =
        let rec Search (str1: string) =
            match str1 with
            | "" -> Error "No common character in group of three"
            | _ ->
                let first = str1.[0]
                let rest = str1.Substring(1)
                if s2.Contains(first) && s3.Contains(first) then Ok first
                else Search rest
        Search s1

    let ScoreGroup (s1: string, s2: string, s3: string) : Result<int, string> =
        FindCommonInGroupsOfThree s1 s2 s3
        |> Result.bind ScoreChar

    let ScoreGroupsOfThree (input: list<string * string * string>) : Result<int, string> =
        input
        |> List.map ScoreGroup
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok sum, Ok score -> Ok (sum + score)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        ) (Ok 0)

    let part1 input =
        match ScoreAllRows input with
        | Ok n -> string n
        | Error msg -> $"Error: {msg}"

    let part2 input =
        match
            input
            |> GetGroupsOfThree
            |> Result.bind ScoreGroupsOfThree
        with
        | Ok n -> string n
        | Error msg -> $"Error: {msg}"

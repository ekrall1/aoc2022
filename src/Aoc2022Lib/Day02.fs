namespace Aoc2022Lib

module Day02 =

    let ConvertShapePart1 (you: string) : Result<string, string> =
        match you with
        | "Y" -> Ok "B"
        | "X" -> Ok "A"
        | "Z" -> Ok "C"
        | _ -> Error "invalid input"

    let ConvertShapePart2 (you: string) (opponent: string) : Result<string, string> =
        match opponent with
        | "A" ->
            if you = "Z" then Ok "B"
            else if you = "X" then Ok "C"
            else Ok "A"
        | "B" ->
            if you = "Z" then Ok "C"
            else if you = "X" then Ok "A"
            else Ok "B"
        | "C" ->
            if you = "Z" then Ok "A"
            else if you = "X" then Ok "B"
            else Ok "C"
        | _ -> Error "invalid value"

    let ScoreSingleMatch (opponent: string) (you: string) : Result<int, string> =
        if opponent.Equals you then
            Ok 3
        else
            match you with
            | "A" -> (if opponent = "C" then 6 else 0) |> Ok
            | "B" -> (if opponent = "A" then 6 else 0) |> Ok
            | "C" -> (if opponent = "B" then 6 else 0) |> Ok
            | _ -> Error "invalid input"

    let ScoreSingleShape (you: string) : Result<int, string> =
        match you with
        | "A" -> Ok 1
        | "B" -> Ok 2
        | "C" -> Ok 3
        | _ -> Error "invalid inpuut"

    let ParseLine (line: string) : Result<string * string, string> =
        let parts = line.Split(' ')

        if parts.Length <> 2 then
            Error $"Invalid input line: {line}"
        else
            Ok(parts.[0], parts.[1])

    let ScoreRound (line: string) (part: int) : Result<int, string> =
        ParseLine line
        |> Result.bind (fun (opponent, youRaw) ->
            let youResult =
                if part = 1 then
                    ConvertShapePart1 youRaw
                else
                    ConvertShapePart2 youRaw opponent

            youResult
            |> Result.bind (fun you ->
                ScoreSingleMatch opponent you
                |> Result.bind (fun matchScore ->
                    ScoreSingleShape you |> Result.map (fun shapeScore -> matchScore + shapeScore))))

    let ScoreAllRounds (input: list<string>) (part: int) : Result<int, string> =
        let folder (acc: Result<int, string>) (line: string) : Result<int, string> =
            acc
            |> Result.bind (fun sum -> ScoreRound line part |> Result.map (fun score -> sum + score))

        List.fold folder (Ok 0) input

    let part1 input =
        input
        |> (fun input -> ScoreAllRounds input 1)
        |> Result.map string
        |> Result.defaultValue "Error calculating part 1"

    let part2 input =
        input
        |> (fun input -> ScoreAllRounds input 2)
        |> Result.map string
        |> Result.defaultValue "Error calculating part 2"

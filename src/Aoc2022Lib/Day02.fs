namespace Aoc2022Lib

module Day02 =

    let ConvertShapePart1 (you: string) : string =
        match you with
        | "Y" -> "B"
        | "X" -> "A"
        | "Z" -> "C"
        | _ -> failwith "invalid input"

    let ConvertShapePart2 (you: string) (opponent: string) : string =
        match opponent with
        | "A" ->
            if you = "Z" then "B"
            else if you = "X" then "C"
            else "A"
        | "B" ->
            if you = "Z" then "C"
            else if you = "X" then "A"
            else "B"
        | "C" ->
            if you = "Z" then "A"
            else if you = "X" then "B"
            else "C"
        | _ -> failwith "invalid value"

    let ScoreSingleMatch (opponent: string) (you: string) : int =
        if opponent.Equals you then
            3
        else
            match you with
            | "A" -> if opponent = "C" then 6 else 0
            | "B" -> if opponent = "A" then 6 else 0
            | "C" -> if opponent = "B" then 6 else 0
            | _ -> failwith "invalid input"

    let ScoreSingleShape (you: string) : int =
        match you with
        | "A" -> 1
        | "B" -> 2
        | "C" -> 3
        | _ -> failwith "invalid inpuut"

    let ScoreAllRounds (input: list<string>) (part: int) =

        let rec SumRounds (lst: list<string>) (acc: int) : int =
            match lst with
            | hd :: tl ->
                let plays: array<string> = hd.Split(separator = [| ' ' |])

                let you =
                    if part = 1 then
                        ConvertShapePart1 plays.[1]
                    else
                        ConvertShapePart2 plays.[1] plays.[0]

                let opponent = plays.[0]
                let matchScore = ScoreSingleMatch opponent you
                let shapeScore = ScoreSingleShape you
                SumRounds tl (acc + matchScore + shapeScore)
            | [] -> acc

        SumRounds input 0

    let part1 input =
        input |> (fun input -> ScoreAllRounds input 1) |> string

    let part2 input =
        input |> (fun input -> ScoreAllRounds input 2) |> string

namespace Aoc2022Lib

module Day02 =

    let ScoreSingleMatch (opponent: string) (you: string) : int =
        match you with 
        | "Y" -> if opponent = "A" then 6 else if opponent = "B" then 3 else 0
        | "X" -> if opponent = "C" then 6 else if opponent = "A" then 3 else 0
        | "Z" -> if opponent = "B" then 6 else if opponent = "C" then 3 else 0
        | _ -> failwith "invalid input"

    let ScoreSingleShape (you: string) : int =
        match you with
        | "X" -> 1
        | "Y" -> 2
        | "Z" -> 3
        | _ -> failwith "invalid inpuut"

    let ScoreAllRounds (input: list<string>) =

        let rec SumRounds (lst: list<string>) (acc: int) : int =
            match lst with
            | hd :: tl -> 
                let plays : array<string> = hd.Split(separator=[| ' ' |])
                let you = plays.[1]
                let opponent = plays.[0]
                let matchScore = ScoreSingleMatch opponent you
                let shapeScore = ScoreSingleShape you
                SumRounds tl (acc + matchScore + shapeScore)
            | [] -> acc
        SumRounds input 0



        

    let part1 input = input |> ScoreAllRounds |> string
    let part2 input = "not implemented"
        
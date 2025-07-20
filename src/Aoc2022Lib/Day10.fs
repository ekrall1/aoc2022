namespace Aoc2022Lib


module Day10 =

    let GetDeterminant (adders: int64 list) (cutoff: int) : int64 =
        adders
        |> List.truncate (cutoff - 1)
        |> List.sum
        |> fun x -> (x + int64 1) * int64 cutoff

    let GetInstructionSum (lines: string list) : int64 =

        let rec findAdders (lst: string list) (acc: int64 list) : (int64 list) =
            match lst with
            | [] -> acc
            | hd :: tl ->
                let parts = hd.Split(' ')

                if parts.[0] = "addx" then
                    findAdders tl (int parts.[1] :: 0 :: acc)
                else
                    findAdders tl (0 :: acc)

        let adders = List.rev (findAdders lines [])
        let add20 = GetDeterminant adders 20
        let add60 = GetDeterminant adders 60
        let add100 = GetDeterminant adders 100
        let add140 = GetDeterminant adders 140
        let add180 = GetDeterminant adders 180
        let add220 = GetDeterminant adders 220

        add20 + add60 + add100 + add140 + add180 + add220


    let part1 (lines: string list) : string =
        let sum = GetInstructionSum lines
        sum.ToString()

    let part2 (lines: string list) : string = "Not implemented yet"

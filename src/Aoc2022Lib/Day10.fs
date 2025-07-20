namespace Aoc2022Lib


module Day10 =

    let GetDeterminant (adders: int64 list) (cutoff: int) : int64 =
        adders
        |> List.truncate (cutoff - 1)
        |> List.sum
        |> fun x -> (x + int64 1) * int64 cutoff

    let rec FindAdders (lst: string list) (acc: int64 list) : (int64 list) =
        match lst with
        | [] -> acc
        | hd :: tl ->
            let parts = hd.Split(' ')

            if parts.[0] = "addx" then
                FindAdders tl (int parts.[1] :: 0 :: acc)
            else
                FindAdders tl (0 :: acc)

    let GetInstructionSum (lines: string list) : int64 =

        let adders = List.rev (FindAdders lines [])
        let add20 = GetDeterminant adders 20
        let add60 = GetDeterminant adders 60
        let add100 = GetDeterminant adders 100
        let add140 = GetDeterminant adders 140
        let add180 = GetDeterminant adders 180
        let add220 = GetDeterminant adders 220

        add20 + add60 + add100 + add140 + add180 + add220


    let SolvePart2 (lines: string list) : string =
        let adders = List.rev (FindAdders lines [])

        let width = 40

        let folder (x: int64, (cycle, row, output)) (delta: int64) =
            let pixelPos = cycle % width
            let spriteRange = x - 1L, x + 1L

            let pixel =
                if int64 pixelPos >= fst spriteRange && int64 pixelPos <= snd spriteRange then
                    '#'
                else
                    '.'

            let newRow = row @ [ pixel ]

            let newOutput, nextRow =
                if pixelPos = width - 1 then
                    output @ [ newRow ], [] // new line
                else
                    output, newRow // Continue line

            let newX = x + delta
            (newX, (cycle + 1, nextRow, newOutput))

        let (_, (_, finalRow, imageRows)) = adders |> List.fold folder (1L, (0, [], []))

        // Add the last row if it wasn't committed
        let allRows =
            if finalRow = [] then
                imageRows
            else
                imageRows @ [ finalRow ]

        allRows
        |> List.map (fun chars -> System.String.Concat(Array.ofList chars))
        |> String.concat "\n"

    let part1 (lines: string list) : string =
        let sum = GetInstructionSum lines
        sum.ToString()

    let part2 (lines: string list) : string = "\n" + SolvePart2 lines

namespace Aoc2022Lib

module Day01 =

    let GetMaxCalories (input: list<string>) : int =
        let rec FindMax (lst: list<string>) (acc: int) (most: int) : int =
            match lst with
            | [] -> most
            | hd :: tl ->
                if hd = "" then
                    let most = max acc most
                    FindMax tl 0 most
                else
                    let acc = acc + int hd
                    FindMax tl acc most
        FindMax input 0 0            
        

    let part1 input = input |> GetMaxCalories |> string
    let part2 input = "not implemented"
        
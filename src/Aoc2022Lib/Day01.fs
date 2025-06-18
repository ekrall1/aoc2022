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

    let GetListOfSums (input: list<string>) : list<int> =
        let rec FlattenSums (lst: list<string>) (acc: list<int>) (cur :int) : list<int> =
            match lst with
            | [] -> cur :: acc
            | hd :: tl -> 
                if hd = "" then
                    FlattenSums tl (cur :: acc) 0
                else
                    FlattenSums tl acc (cur + int hd)
        FlattenSums input [] 0

    let GetTopNSum (lst: list<int>) (n : int) : int =
        List.sortDescending lst |> (fun lst -> List.take n lst) |> List.sum
        

    let part1 input = input |> GetMaxCalories |> string
    let part2 input = input |> GetListOfSums |> (fun lst -> GetTopNSum lst 3) |> string
        
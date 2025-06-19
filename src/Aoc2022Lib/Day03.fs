namespace Aoc2022Lib

module Day03 =

    let SplitInHalf (s: string) : string * string =
        if s.Length % 2 <> 0 then
            invalidArg "s" "Input string must have an even number of characters"
        else
            let half = s.Length / 2
            s.Substring(0, half), s.Substring(half)

    let FindCommon (s1: string) (s2: string) : char =

        let rec Search (str1 : string) : char =
            match str1 with
            | "" -> failwith "no characters to check"
            | _ -> 
                let first = str1.[0]
                let rest = str1.Substring(1)
                if s2.Contains(first) then first else Search rest
        Search s1

    let ScoreChar (c: char) : int =
        match c with
        | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
        | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 27
        | _ -> invalidArg "c" "Character must be a-z or A-Z"


    let ScoreAllRows (input: list<string>) : int =
        
        let rec FindSum (lst: list<string>) (acc: int) : int =
            match lst with 
            | [] -> acc
            | hd :: tl -> 
                let cur = hd |> SplitInHalf |> fun (s1, s2) -> FindCommon s1 s2 |> ScoreChar
                FindSum tl (acc + cur)
        FindSum input 0



    let part1 input =
        input |> (fun input -> ScoreAllRows input) |> string

    let part2 input =
        failwith "not implemented yet"

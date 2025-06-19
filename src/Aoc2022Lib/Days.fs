namespace Aoc2022Lib

module Days =

    let getDay day : (list<string> -> string) * (list<string> -> string) =
        match day with
        | 1 -> Day01.part1, Day01.part2
        | 2 -> Day02.part1, Day02.part2
        | _ -> failwithf "Day %d not implemented" day

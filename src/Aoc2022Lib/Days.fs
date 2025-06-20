namespace Aoc2022Lib

module Days =

    let getDay day : (list<string> -> string) * (list<string> -> string) =
        match day with
        | 1 -> Day01.part1, Day01.part2
        | 2 -> Day02.part1, Day02.part2
        | 3 -> Day03.part1, Day03.part2
        | 4 -> Day04.part1, Day04.part2
        | 5 -> Day05.part1, Day05.part2
        | _ -> failwithf "Day %d not implemented" day

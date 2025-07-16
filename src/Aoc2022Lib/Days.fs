namespace Aoc2022Lib

module Days =

    let getDay day : (list<string> -> string) * (list<string> -> string) =
        match day with
        | 1 -> Day01.part1, Day01.part2
        | 2 -> Day02.part1, Day02.part2
        | 3 -> Day03.part1, Day03.part2
        | 4 -> Day04.part1, Day04.part2
        | 5 -> Day05.part1, Day05.part2
        | 6 -> Day06.part1, Day06.part2
        | 7 -> Day07.part1, Day07.part2
        | 8 -> Day08.part1, Day08.part2
        | 9 -> Day09.part1, Day09.part2
        | _ -> failwithf "Day %d not implemented" day

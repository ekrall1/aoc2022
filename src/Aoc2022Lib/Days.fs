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
        | 10 -> Day10.part1, Day10.part2
        | 11 -> Day11.part1, Day11.part2
        | 12 -> Day12.part1, Day12.part2
        | 13 -> Day13.part1, Day13.part2
        | 14 -> Day14.part1, Day14.part2
        | 15 -> Day15.part1, Day15.part2
        | 16 -> Day16.part1, Day16.part2
        | 17 -> Day17.part1, Day17.part2
        | 18 -> Day18.part1, Day18.part2
        | 19 -> Day19.part1, Day19.part2
        | _ -> failwithf "Day %d not implemented" day

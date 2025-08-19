namespace Aoc2022Lib

type DayRunner =
    { part1: list<string> -> string
      part2: list<string> -> string }

module Dispatch =
    let wrap (day: int) : DayRunner =
        let p1, p2 = Days.getDay day
        { part1 = p1; part2 = p2 }

    let runners: Map<int, DayRunner> =
        [ 1, wrap 1
          2, wrap 2
          3, wrap 3
          4, wrap 4
          5, wrap 5
          6, wrap 6
          7, wrap 7
          8, wrap 8
          9, wrap 9
          10, wrap 10
          11, wrap 11
          12, wrap 12
          13, wrap 13
          14, wrap 14 ]
        |> Map.ofList

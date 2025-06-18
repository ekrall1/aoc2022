namespace Aoc2022Tests

open Aoc2022Lib
open Xunit

module UnitTests =

    let readTestInput (filename: string) : string list =
        let baseDir = __SOURCE_DIRECTORY__  // directory of the source .fs file
        let path = System.IO.Path.Combine(baseDir, "TestInput", filename)
        ReadInput.read path

    [<Fact>]
    let ``Day 1 Part 1`` () =
        let input = readTestInput "Day01.txt"
        let result = Dispatch.wrap 1
                    |> fun runner -> runner.part1 input
        Assert.Equal("24000", result)


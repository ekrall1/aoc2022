namespace Aoc2022Tests

open Aoc2022Lib
open Xunit

module UnitTests =

    let readTestInput (filename: string) : string list =
        let baseDir = __SOURCE_DIRECTORY__ // directory of the source .fs file
        let path = System.IO.Path.Combine(baseDir, "TestInput", filename)
        ReadInput.read path

    [<Fact>]
    let ``Day 1 Part 1`` () =
        let input = readTestInput "Day01.txt"
        let result = Dispatch.wrap 1 |> fun runner -> runner.part1 input
        Assert.Equal("24000", result)

    [<Fact>]
    let ``Day 1 Part 2`` () =
        let input = readTestInput "Day01.txt"
        let result = Dispatch.wrap 1 |> fun runner -> runner.part2 input
        Assert.Equal("45000", result)

    [<Fact>]
    let ``Day 2 Part 1`` () =
        let input = readTestInput "Day02.txt"
        let result = Dispatch.wrap 2 |> fun runner -> runner.part1 input
        Assert.Equal("15", result)

    [<Fact>]
    let ``Day 2 Part 2`` () =
        let input = readTestInput "Day02.txt"
        let result = Dispatch.wrap 2 |> fun runner -> runner.part2 input
        Assert.Equal("12", result)

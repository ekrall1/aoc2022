﻿namespace Aoc2022Tests

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

    [<Fact>]
    let ``Day 3 Part 1`` () =
        let input = readTestInput "Day03.txt"
        let result = Dispatch.wrap 3 |> fun runner -> runner.part1 input
        Assert.Equal("157", result)

    [<Fact>]
    let ``Day 3 Part 2`` () =
        let input = readTestInput "Day03.txt"
        let result = Dispatch.wrap 3 |> fun runner -> runner.part2 input
        Assert.Equal("70", result)

    [<Fact>]
    let ``Day 4 Part 1`` () =
        let input = readTestInput "Day04.txt"
        let result = Dispatch.wrap 4 |> fun runner -> runner.part1 input
        Assert.Equal("2", result)

    [<Fact>]
    let ``Day 4 Part 2`` () =
        let input = readTestInput "Day04.txt"
        let result = Dispatch.wrap 4 |> fun runner -> runner.part2 input
        Assert.Equal("4", result)

    [<Fact>]
    let ``Day 5 Part 1`` () =
        let input = readTestInput "Day05.txt"
        let result = Dispatch.wrap 5 |> fun runner -> runner.part1 input
        Assert.Equal("CMZ", result)

    [<Fact>]
    let ``Day 5 Part 2`` () =
        let input = readTestInput "Day05.txt"
        let result = Dispatch.wrap 5 |> fun runner -> runner.part2 input
        Assert.Equal("MCD", result)

    [<Fact>]
    let ``Day 6 Part 1`` () =
        let input = readTestInput "Day06.txt"
        let result = Dispatch.wrap 6 |> fun runner -> runner.part1 input
        Assert.Equal("39", result)

    [<Fact>]
    let ``Day 6 Part 2`` () =
        let input = readTestInput "Day06.txt"
        let result = Dispatch.wrap 6 |> fun runner -> runner.part2 input
        Assert.Equal("120", result)

    [<Fact>]
    let ``Day 7 Part 1`` () =
        let input = readTestInput "Day07.txt"
        let result = Dispatch.wrap 7 |> fun runner -> runner.part1 input
        Assert.Equal("95437", result)

    [<Fact>]
    let ``Day 7 Part 2`` () =
        let input = readTestInput "Day07.txt"
        let result = Dispatch.wrap 7 |> fun runner -> runner.part2 input
        Assert.Equal("24933642", result)

    [<Fact>]
    let ``Day 8 Part 1`` () =
        let input = readTestInput "Day08.txt"
        let result = Dispatch.wrap 8 |> fun runner -> runner.part1 input
        Assert.Equal("21", result)


    [<Fact>]
    let ``Day 8 Part 2`` () =
        let input = readTestInput "Day08.txt"
        let result = Dispatch.wrap 8 |> fun runner -> runner.part2 input
        Assert.Equal("8", result)

    [<Fact>]
    let ``Day 9 Part 1`` () =
        let input = readTestInput "Day09.txt"
        let result = Dispatch.wrap 9 |> fun runner -> runner.part1 input
        Assert.Equal("13", result)

    [<Fact>]
    let ``Day 9 Part 2`` () =
        let input = readTestInput "Day09.txt"
        let result = Dispatch.wrap 9 |> fun runner -> runner.part2 input
        Assert.Equal("1", result)

    [<Fact>]
    let ``Day 9 Part 2 Test 2`` () =
        let input = readTestInput "Day09_Test2.txt"
        let result = Dispatch.wrap 9 |> fun runner -> runner.part2 input
        Assert.Equal("36", result)

    [<Fact>]
    let ``Day 10 Part 1`` () =
        let input = readTestInput "Day10.txt"
        let result = Dispatch.wrap 10 |> fun runner -> runner.part1 input
        Assert.Equal("13140", result)

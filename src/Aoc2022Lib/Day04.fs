namespace Aoc2022Lib

open Aoc2022Lib.Utils

module Day04 =

    let ParseRange (parts: string[]) : Result<int * int, string> =
        if parts.Length <> 2 then
            Error $"Invalid assignment for ParseRange"
        else
            let r1 = parseInt parts.[0]
            let r2 = parseInt parts.[1]
            map2 (fun a b -> (a, b)) r1 r2

    let GetAssignments (s1: string) (s2: string) : Result<list<int * int>, string> =
        let parts1 = s1.Split('-')
        let parts2 = s2.Split('-')

        match ParseRange parts1, ParseRange parts2 with
        | Ok r1, Ok r2 -> Ok [ r1; r2 ]
        | Error e, _ -> Error e
        | _, Error e -> Error e


    let ParseLine (s: string) : Result<(int * int) list, string> =
        let parts = s.Split([| ',' |])

        if not (parts.Length.Equals 2) then
            Error $"Invalid input {s}"
        else
            GetAssignments parts.[0] parts.[1]

    let ScoreSingleRange (lst: (int * int) list) : Result<int, string> =
        match lst with
        | [ (a1, a2); (b1, b2) ] ->
            let isAInB = a1 >= b1 && a2 <= b2
            let isBInA = b1 >= a1 && b2 <= a2
            if isAInB || isBInA then Ok 1 else Ok 0
        | _ -> Error "Row does not have two assignments"

    let ScoreAllRows (input: list<string>) : Result<int, string> =
        input
        |> List.map ParseLine
        |> List.fold
            (fun accRes lineRes ->
                accRes
                |> Result.bind (fun acc ->
                    lineRes |> Result.bind ScoreSingleRange |> Result.map (fun score -> acc + score)))
            (Ok 0)

    let part1 input =
        match ScoreAllRows input with
        | Ok n -> string n
        | Error msg -> $"Error: {msg}"

    let part2 input = "not implemented"

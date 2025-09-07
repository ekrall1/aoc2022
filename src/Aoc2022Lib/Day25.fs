/// <summary>
/// Advent of Code 2022 — Day 25
/// </summary>
module Aoc2022Lib.Day25

let snafu =
    function
    | '=' -> -2.
    | '-' -> -1.
    | '0' -> 0.
    | '1' -> 1.
    | '2' -> 2.
    | _ -> failwith "invalid argument to snafu"

let snafuRev =
    function
    | 0L -> '='
    | 1L -> '-'
    | 2L -> '0'
    | 3L -> '1'
    | 4L -> '2'
    | x -> failwith $"invalid argument to snafuRev {x}"

let snafuToNum (line: string) =
    line |> Seq.rev |> Seq.mapi (fun i c -> 5. ** i * snafu c) |> Seq.sum

let numToSnafu (num: int64) =

    let rec loop (n: int64) (acc: string) =
        match n with
        | 0L -> acc
        | _ ->
            let nextIdx = (n + 2L) % 5L
            let nextN = if nextIdx < 2L then (n + 5L) / 5L else (n / 5L) // account for carry
            let nextAcc = string (snafuRev nextIdx) + acc
            loop nextN nextAcc

    loop num ""

let part1 (lines: string list) : string =
    lines
    |> Seq.map (fun line -> snafuToNum line)
    |> Seq.fold (fun acc n -> acc + n) 0.
    |> int64
    |> numToSnafu

let part2 (lines: string list) : string = "not implemented"

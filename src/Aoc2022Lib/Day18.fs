/// <summary>
/// Advent of Code 2022 — Day 18
/// </summary>
module Aoc2022Lib.Day18

open System
open System.Text.RegularExpressions

type Cube = Cube of int * int * int
type ParseError = InvalidLine of string

module Cube =
    let create (x: int) (y: int) (z: int) = Cube(x, y, z)

    let x (Cube(x, y, z)) = x
    let y (Cube(x, y, z)) = y
    let z (Cube(x, y, z)) = z

// ---------------- parsing input ----------------
let parseRow (s: string) : Result<Cube, ParseError> =
    if System.String.IsNullOrWhiteSpace(s) then
        Error(InvalidLine s)
    else
        let pattern = @"(\d+),(\d+),(\d+)"

        let m = Regex.Match(s, pattern)

        if m.Success then
            let x = int m.Groups.[1].Value
            let y = int m.Groups.[2].Value
            let z = int m.Groups.[3].Value
            Ok(Cube.create x y z)
        else
            Error(InvalidLine s)

let rec createInput (lines: string list) (acc: Cube list) : Result<Cube list, ParseError> =
    match lines with
    | [] -> Ok acc
    | head :: tail ->
        match parseRow head with
        | Ok cube -> createInput tail (cube :: acc)
        | Error err -> Error err

// ---------------- constants ----------------
let CubeNeighbors =
    [ (1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1) ]

// ---------------- part 1 soln ----------------
let findExposedSurfaceArea (lst: Cube list) =
    let cubeSet = Set.ofList lst

    let countCoveredSides (cube: Cube) =
        List.map
            (fun (dx, dy, dz) ->
                let neighbor = Cube.create (Cube.x cube + dx) (Cube.y cube + dy) (Cube.z cube + dz)
                if Set.contains neighbor cubeSet then 1 else 0)
            CubeNeighbors
        |> List.sum

    List.fold (fun acc cube -> acc + (6 - countCoveredSides cube)) 0 lst


let part1 (lines: string list) =
    let cubes = createInput lines []

    match cubes with
    | Ok cubeList -> findExposedSurfaceArea cubeList |> string
    | Error err -> err.ToString()

let part2 (lines: string list) = "not implemented"

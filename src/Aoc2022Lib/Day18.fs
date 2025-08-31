/// <summary>
/// Advent of Code 2022 — Day 18
/// </summary>
module Aoc2022Lib.Day18

open System
open System.Text.RegularExpressions

type Cube = Cube of int * int * int
type MaxMinCoords = MaxMinCoords of int * int
type XYZMaxMinCoords = XYZMaxMinCoords of MaxMinCoords * MaxMinCoords * MaxMinCoords
type ParseError = InvalidLine of string

module Cube =
    let create (x: int) (y: int) (z: int) = Cube(x, y, z)

    let x (Cube(x, y, z)) = x
    let y (Cube(x, y, z)) = y
    let z (Cube(x, y, z)) = z

module MaxMinCoords =
    let create (min: int) (max: int) = MaxMinCoords(min, max)
    let max (MaxMinCoords(min, max)) = max
    let min (MaxMinCoords(min, max)) = min

module XYZMaxMinCoords =
    let create (xCoords: MaxMinCoords) (yCoords: MaxMinCoords) (zCoords: MaxMinCoords) =
        XYZMaxMinCoords(xCoords, yCoords, zCoords)

    let x (XYZMaxMinCoords(x, y, z)) = x
    let y (XYZMaxMinCoords(x, y, z)) = y
    let z (XYZMaxMinCoords(x, y, z)) = z

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

// ---------------- part 2 soln ----------------
let getMaxMinCoordsAirBoundary (lst: Cube list) =
    let xs = List.map Cube.x lst
    let ys = List.map Cube.y lst
    let zs = List.map Cube.z lst

    let xCoords = MaxMinCoords.create (List.min xs - 1) (List.max xs + 1)
    let yCoords = MaxMinCoords.create (List.min ys - 1) (List.max ys + 1)
    let zCoords = MaxMinCoords.create (List.min zs - 1) (List.max zs + 1)
    XYZMaxMinCoords.create xCoords yCoords zCoords

let inBoundary (coord: int * int * int) (boundary: XYZMaxMinCoords) =
    let (x, y, z) = coord

    x >= (MaxMinCoords.min (XYZMaxMinCoords.x boundary))
    && x <= (MaxMinCoords.max (XYZMaxMinCoords.x boundary))
    && y >= (MaxMinCoords.min (XYZMaxMinCoords.y boundary))
    && y <= (MaxMinCoords.max (XYZMaxMinCoords.y boundary))
    && z >= (MaxMinCoords.min (XYZMaxMinCoords.z boundary))
    && z <= (MaxMinCoords.max (XYZMaxMinCoords.z boundary))

let updateSearchParams
    (hd: (int * int * int))
    (lst: (int * int * int) list)
    (visited: Set<int * int * int>)
    (area: int)
    (boundary)
    (cubeSet: Set<Cube>)
    =
    CubeNeighbors
    |> Seq.fold
        (fun (qAcc, vAcc, aAcc) (dx, dy, dz) ->
            let (x, y, z) = hd
            let neighbor = (x + dx, y + dy, z + dz)

            if inBoundary neighbor boundary && not (Set.contains neighbor vAcc) then
                let (x', y', z') = neighbor

                if Set.contains (Cube.create x' y' z') cubeSet then
                    (qAcc, vAcc, aAcc + 1)
                else
                    (neighbor :: qAcc, vAcc.Add neighbor, aAcc)
            else
                (qAcc, vAcc, aAcc))
        (lst, visited, area)

let bfs
    (queue: (int * int * int) list)
    (visited: Set<int * int * int>)
    (boundary: XYZMaxMinCoords)
    (area: int)
    (cubeSet: Set<Cube>)
    =
    let rec loop (queue: (int * int * int) list) (visited: Set<int * int * int>) (area: int) =
        match queue with
        | [] -> area
        | hd :: tl ->
            let tl', visited', area' = updateSearchParams hd tl visited area boundary cubeSet
            loop tl' visited' area'

    loop queue visited area


let findOuterSurfaceArea (lst: Cube list) =
    let cubeSet = Set.ofList lst
    let boundaryCoords = getMaxMinCoordsAirBoundary lst

    let minCoords =
        (MaxMinCoords.min (XYZMaxMinCoords.x boundaryCoords),
         MaxMinCoords.min (XYZMaxMinCoords.y boundaryCoords),
         MaxMinCoords.min (XYZMaxMinCoords.z boundaryCoords))

    let queue = [ minCoords ]
    let visited = Set.empty.Add(minCoords)

    bfs queue visited boundaryCoords 0 cubeSet

let part1 (lines: string list) =
    let cubes = createInput lines []

    match cubes with
    | Ok cubeList -> findExposedSurfaceArea cubeList |> string
    | Error err -> err.ToString()

let part2 (lines: string list) =
    let cubes = createInput lines []

    match cubes with
    | Ok cubeList -> findOuterSurfaceArea cubeList |> string
    | Error err -> err.ToString()

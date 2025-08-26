module Aoc2022Lib.Day14

open Aoc2022Lib.Railway

type Point = int * int
type Cave = Set<Point>

type ParseError =
    | InvalidCoordinate of string
    | InvalidLine of string

type SimulationError =
    | SandFellIntoVoid
    | InvalidSandPosition

// Railway-oriented parsing functions
let parseCoordinate (coord: string) : Result<Point, ParseError> =
    match coord.Split(',') with
    | [| x; y |] ->
        match System.Int32.TryParse(x), System.Int32.TryParse(y) with
        | (true, xVal), (true, yVal) -> Ok(xVal, yVal)
        | _ -> Error(InvalidCoordinate coord)
    | _ -> Error(InvalidCoordinate coord)

let parseCoordinates (coordStr: string) : Result<Point list, ParseError> =
    coordStr.Split(" -> ")
    |> Array.toList
    |> List.map parseCoordinate
    |> List.fold (fun acc result -> acc >>= fun points -> result >>= fun point -> Ok(point :: points)) (Ok [])
    <!> List.rev

let drawLine (p1: Point) (p2: Point) : Point list =
    let (x1, y1) = p1
    let (x2, y2) = p2

    if x1 = x2 then
        // Vertical line
        let minY, maxY = min y1 y2, max y1 y2
        [ minY..maxY ] |> List.map (fun y -> (x1, y))
    else
        // Horizontal line
        let minX, maxX = min x1 x2, max x1 x2
        [ minX..maxX ] |> List.map (fun x -> (x, y1))

let drawRockStructure (points: Point list) : Result<Point list, ParseError> =
    match points with
    | [] -> Ok []
    | [ _ ] -> Ok points
    | _ -> points |> List.pairwise |> List.collect (fun (p1, p2) -> drawLine p1 p2) |> Ok

let parseLine (line: string) : Result<Point list, ParseError> =
    if System.String.IsNullOrWhiteSpace(line) then
        Ok []
    else
        parseCoordinates line >>= drawRockStructure

let buildCave (lines: string list) : Result<Cave, ParseError> =
    lines
    |> List.map parseLine
    |> List.fold
        (fun acc result ->
            acc
            >>= fun cave -> result >>= fun points -> Ok(Set.union cave (Set.ofList points)))
        (Ok Set.empty)

let findMaxY (cave: Cave) : int =
    if Set.isEmpty cave then
        0
    else
        cave |> Set.map snd |> Set.maxElement

let dropSand (cave: Cave) (maxY: int) (sandStart: Point) : Result<Point option, SimulationError> =
    let rec fall (x, y) =
        if y > maxY then
            Error SandFellIntoVoid
        else
            let below = (x, y + 1)
            let belowLeft = (x - 1, y + 1)
            let belowRight = (x + 1, y + 1)

            if not (Set.contains below cave) then fall below
            elif not (Set.contains belowLeft cave) then fall belowLeft
            elif not (Set.contains belowRight cave) then fall belowRight
            else Ok(Some(x, y))

    fall sandStart

let dropSandWithFloor (cave: Cave) (floorY: int) (sandStart: Point) : Result<Point option, SimulationError> =
    let rec fall (x, y) =
        let below = (x, y + 1)
        let belowLeft = (x - 1, y + 1)
        let belowRight = (x + 1, y + 1)

        // Check if we've reached the floor
        if y + 1 = floorY then Ok(Some(x, y))
        elif not (Set.contains below cave) then fall below
        elif not (Set.contains belowLeft cave) then fall belowLeft
        elif not (Set.contains belowRight cave) then fall belowRight
        else Ok(Some(x, y))

    fall sandStart

let simulateSand (cave: Cave) : Result<int, SimulationError> =
    let maxY = findMaxY cave
    let sandStart = (500, 0)

    let rec simulate currentCave count =
        match dropSand currentCave maxY sandStart with
        | Error SandFellIntoVoid -> Ok count // Return count when sand falls into void
        | Error err -> Error err
        | Ok None -> Ok count // This shouldn't happen but handle it
        | Ok(Some sandPos) ->
            let newCave = Set.add sandPos currentCave
            simulate newCave (count + 1)

    simulate cave 0

let simulateSandWithFloor (cave: Cave) : Result<int, SimulationError> =
    let maxY = findMaxY cave
    let floorY = maxY + 2 // Floor is 2 units below the lowest rock
    let sandStart = (500, 0)

    let rec simulate currentCave count =
        // Check if the source is blocked
        if Set.contains sandStart currentCave then
            Ok count
        else
            match dropSandWithFloor currentCave floorY sandStart with
            | Error err -> Error err
            | Ok None -> Ok count // This shouldn't happen but handle it
            | Ok(Some sandPos) ->
                let newCave = Set.add sandPos currentCave
                simulate newCave (count + 1)

    simulate cave 0

let part1 (lines: string list) : string =
    match buildCave lines with
    | Error(InvalidCoordinate coord) -> $"Error parsing coordinate: {coord}"
    | Error(InvalidLine line) -> $"Error parsing line: {line}"
    | Ok cave ->
        match simulateSand cave with
        | Error SandFellIntoVoid -> "0" // This shouldn't happen in normal cases
        | Error InvalidSandPosition -> "Error: Invalid sand position"
        | Ok count -> string count

let part2 (lines: string list) : string =
    match buildCave lines with
    | Error(InvalidCoordinate coord) -> $"Error parsing coordinate: {coord}"
    | Error(InvalidLine line) -> $"Error parsing line: {line}"
    | Ok cave ->
        match simulateSandWithFloor cave with
        | Error SandFellIntoVoid -> "Error: Sand fell into void (shouldn't happen with floor)"
        | Error InvalidSandPosition -> "Error: Invalid sand position"
        | Ok count -> string count

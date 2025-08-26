module Aoc2022Lib.Day15

open System.Text.RegularExpressions
open Aoc2022Lib.Railway

type Point = int * int

type Sensor =
    { Position: Point
      ClosestBeacon: Point
      Distance: int }

type Range = { Start: int; End: int }

type ParseError =
    | InvalidLine of string
    | EmptyInput

type SolutionError =
    | ParseError of ParseError
    | NoSensorsFound
    | InvalidTargetRow

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parseLine (line: string) : Result<Sensor, ParseError> =
    if System.String.IsNullOrWhiteSpace(line) then
        Error(InvalidLine line)
    else
        let pattern =
            @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"

        let m = Regex.Match(line, pattern)

        if m.Success then
            let sx = int m.Groups.[1].Value
            let sy = int m.Groups.[2].Value
            let bx = int m.Groups.[3].Value
            let by = int m.Groups.[4].Value
            let sensorPos = (sx, sy)
            let beaconPos = (bx, by)
            let distance = manhattanDistance sensorPos beaconPos

            Ok
                { Position = sensorPos
                  ClosestBeacon = beaconPos
                  Distance = distance }
        else
            Error(InvalidLine line)

let parseSensors (lines: string list) : Result<Sensor list, ParseError> =
    if List.isEmpty lines then
        Error EmptyInput
    else
        lines
        |> List.filter (fun line -> not (System.String.IsNullOrWhiteSpace(line)))
        |> List.map parseLine
        |> List.fold (fun acc result -> acc >>= fun sensors -> result >>= fun sensor -> Ok(sensor :: sensors)) (Ok [])
        <!> List.rev

let getCoverageRangeForRow (sensor: Sensor) (targetY: int) : Range option =
    let (sx, sy) = sensor.Position
    let distanceToRow = abs (sy - targetY)

    if distanceToRow > sensor.Distance then
        None // Sensor's exclusion zone doesn't reach this row
    else
        let remainingDistance = sensor.Distance - distanceToRow
        let startX = sx - remainingDistance
        let endX = sx + remainingDistance
        Some { Start = startX; End = endX }

let mergeRanges (ranges: Range list) : Range list =
    let sortedRanges = ranges |> List.sortBy (fun r -> r.Start)

    let rec merge acc current remaining =
        match remaining with
        | [] -> current :: acc
        | next :: rest ->
            if next.Start <= current.End + 1 then
                // Ranges overlap or are adjacent, merge them
                let merged =
                    { Start = current.Start
                      End = max current.End next.End }

                merge acc merged rest
            else
                // No overlap, add current to result and continue with next
                merge (current :: acc) next rest

    match sortedRanges with
    | [] -> []
    | first :: rest -> merge [] first rest |> List.rev

let countPositionsInRanges (ranges: Range list) : int =
    ranges |> List.sumBy (fun range -> range.End - range.Start + 1)

let getBeaconsInRow (sensors: Sensor list) (targetY: int) : Set<int> =
    sensors
    |> List.map (fun s -> s.ClosestBeacon)
    |> List.filter (fun (_, y) -> y = targetY)
    |> List.map fst
    |> Set.ofList

let countExcludedPositions (sensors: Sensor list) (targetY: int) : int =
    // Get coverage ranges for the target row
    let coverageRanges =
        sensors
        |> List.choose (fun sensor -> getCoverageRangeForRow sensor targetY)
        |> mergeRanges

    // Count total positions covered by exclusion zones
    let totalCovered = countPositionsInRanges coverageRanges

    // Get beacon positions that actually exist in this row
    let beaconsInRow = getBeaconsInRow sensors targetY

    // Count how many actual beacon positions are within our coverage ranges
    let beaconsInCoverage =
        beaconsInRow
        |> Set.filter (fun beaconX ->
            coverageRanges
            |> List.exists (fun range -> beaconX >= range.Start && beaconX <= range.End))
        |> Set.count

    totalCovered - beaconsInCoverage

// Determine target row based on input size (heuristic)
let getTargetRow (sensors: Sensor list) : int =
    let maxY = sensors |> List.map (fun s -> snd s.Position) |> List.max
    let minY = sensors |> List.map (fun s -> snd s.Position) |> List.min

    // If the data range is small (like test data), use y=10
    if maxY - minY < 100 then 10 else 2000000

let solvePart1 (sensors: Sensor list) : Result<int, SolutionError> =
    if List.isEmpty sensors then
        Error NoSensorsFound
    else
        let targetY = getTargetRow sensors
        let excludedCount = countExcludedPositions sensors targetY
        Ok excludedCount

let part1 (lines: string list) : string =
    match parseSensors lines with
    | Error(InvalidLine line) -> $"Error parsing line: {line}"
    | Error EmptyInput -> "Error: Empty input"
    | Ok sensors ->
        match solvePart1 sensors with
        | Error NoSensorsFound -> "Error: No sensors found"
        | Error InvalidTargetRow -> "Error: Invalid target row"
        | Error(ParseError err) -> $"Parse error: {err}"
        | Ok count -> string count

// For part 2, we need to find the distress beacon
let findDistressBeacon (sensors: Sensor list) (maxCoord: int) : Point option =
    let isValidPosition (x, y) =
        x >= 0
        && x <= maxCoord
        && y >= 0
        && y <= maxCoord
        && sensors
           |> List.forall (fun sensor -> manhattanDistance sensor.Position (x, y) > sensor.Distance)

    // Check the perimeter of each sensor's coverage area
    let rec checkPerimeter sensors =
        match sensors with
        | [] -> None
        | sensor :: rest ->
            let (sx, sy) = sensor.Position
            let distance = sensor.Distance + 1 // Just outside the coverage

            // Check points on the diamond perimeter
            let mutable found = None
            let mutable dy = 0

            while found.IsNone && dy <= distance do
                let dx = distance - dy

                let candidates =
                    [ (sx + dx, sy + dy)
                      (sx + dx, sy - dy)
                      (sx - dx, sy + dy)
                      (sx - dx, sy - dy) ]

                match candidates |> List.tryFind isValidPosition with
                | Some pos -> found <- Some pos
                | None -> dy <- dy + 1

            match found with
            | Some pos -> Some pos
            | None -> checkPerimeter rest

    checkPerimeter sensors

let getMaxCoord (sensors: Sensor list) : int =
    let maxY = sensors |> List.map (fun s -> snd s.Position) |> List.max
    let minY = sensors |> List.map (fun s -> snd s.Position) |> List.min

    // If the data range is small (like test data), use 20
    // Otherwise use 4000000 for the real puzzle
    if maxY - minY < 100 then 20 else 4000000

let solvePart2 (sensors: Sensor list) : Result<int64, SolutionError> =
    if List.isEmpty sensors then
        Error NoSensorsFound
    else
        let maxCoord = getMaxCoord sensors

        match findDistressBeacon sensors maxCoord with
        | Some(x, y) ->
            let tuningFrequency = int64 x * 4000000L + int64 y
            Ok tuningFrequency
        | None -> Error InvalidTargetRow // Reusing this error for "beacon not found"

let part2 (lines: string list) : string =
    match parseSensors lines with
    | Error(InvalidLine line) -> $"Error parsing line: {line}"
    | Error EmptyInput -> "Error: Empty input"
    | Ok sensors ->
        match solvePart2 sensors with
        | Error NoSensorsFound -> "Error: No sensors found"
        | Error InvalidTargetRow -> "Distress beacon not found"
        | Error(ParseError err) -> $"Parse error: {err}"
        | Ok frequency -> string frequency

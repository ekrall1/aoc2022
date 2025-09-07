/// <summary>
/// Advent of Code 2022 — Day 24: Blizzard Basin
/// </summary>
module Aoc2022Lib.Day24

open System
open System.Collections.Generic

/// Row/Col position
type Position = int * int

/// Cardinal directions
type Dir =
    | Up
    | Down
    | Left
    | Right

type Bounds = { Rows: int; Cols: int }

type Blizzard = { Pos: Position; Dir: Dir }

type Basin =
    { Bounds: Bounds
      Walls: Set<Position>
      Blizzards: Blizzard list }

let rec blizzards (lst: string list) (acc: Blizzard list) (row: int) =
    match lst with
    | [] -> acc
    | hd :: tl ->
        let mutable acc = acc

        Seq.iteri
            (fun col ch ->
                match ch with
                | '^' -> acc <- { Pos = (row, col); Dir = Up } :: acc
                | 'v' -> acc <- { Pos = (row, col); Dir = Down } :: acc
                | '<' -> acc <- { Pos = (row, col); Dir = Left } :: acc
                | '>' -> acc <- { Pos = (row, col); Dir = Right } :: acc
                | _ -> ())
            hd

        blizzards tl acc (row + 1)

let walls (allLines: string list) =
    let totalRows = List.length allLines
    let mutable start = (0, 0)
    let mutable goal = (0, 0)

    let rec loop (lst: string list) (acc: Set<Position>) (row: int) =
        match lst with
        | [] -> acc
        | hd :: tl ->
            let mutable acc = acc

            Seq.iteri
                (fun col ch ->
                    if ch = '#' then
                        acc <- acc.Add(row, col)
                    elif ch = '.' && row = 0 then
                        start <- (row, col)
                    elif ch = '.' && row = (totalRows - 1) then
                        goal <- (row, col))
                hd

            loop tl acc (row + 1)

    let wallSet = loop allLines Set.empty 0
    wallSet, start, goal

/// Parse the input lines into the basin state (walls, blizzards, bounds),
/// plus infer the start and goal positions.
let parseInput (lines: string list) : Basin * Position * Position =

    let rows = List.length lines
    let cols = if rows = 0 then 0 else (List.head lines).Length

    let wallSet, start, goal = walls lines
    let blizz = blizzards lines [] 0

    ({ Bounds = { Rows = rows; Cols = cols }
       Walls = wallSet
       Blizzards = blizz },
     start,
     goal)

/// Advance one blizzard one step
let stepBlizzard (bounds: Bounds) (b: Blizzard) : Blizzard =
    let (r, c) = b.Pos

    match b.Dir with
    | Up ->
        { b with
            Pos = ((r - 2 + (bounds.Rows - 2)) % (bounds.Rows - 2) + 1, c) }
    | Down ->
        { b with
            Pos = (r % (bounds.Rows - 2) + 1, c) }
    | Left ->
        { b with
            Pos = (r, (c - 2 + (bounds.Cols - 2)) % (bounds.Cols - 2) + 1) }
    | Right ->
        { b with
            Pos = (r, c % (bounds.Cols - 2) + 1) }

/// Advance all blizzards one step.
let advanceBlizzards (bounds: Bounds) (blizzards: Blizzard list) : Blizzard list =
    blizzards |> List.map (fun b -> stepBlizzard bounds b)

/// Neighbor positions
let candidateMoves ((r, c): Position) : Position list =
    [ (r, c); (r + 1, c); (r, c + 1); (r - 1, c); (r, c - 1) ]

let isCellFree (basin: Basin) ((r, c): Position) : bool =

    r <= basin.Bounds.Rows - 2 && c <= basin.Bounds.Cols - 2 && r >= 1 && c >= 1

/// Is a cell free of blizzards
let isBlizzardFree (blizzards: Blizzard list) (pos: Position) : bool =
    blizzards |> List.exists (fun b -> pos = b.Pos) |> not

/// Find earliest arrival minute from start->goal starting at time t0
let earliestArrival
    (basin: Basin)
    (initialBlizzards: Blizzard list)
    (t0: int)
    (startPos: Position)
    (goalPos: Position)
    =

    let blizzardCache = Dictionary<int, Blizzard list>()
    blizzardCache[t0] <- initialBlizzards

    let positionCache = Dictionary<int, Set<Position>>()

    let blizzardsAt (t: int) =
        if blizzardCache.ContainsKey(t) then
            blizzardCache.[t]
        else
            // get largest t and advance
            let mutable maxT = blizzardCache.Keys |> Seq.max

            while maxT < t do
                let next = advanceBlizzards basin.Bounds blizzardCache.[maxT]
                blizzardCache[maxT + 1] <- next
                maxT <- maxT + 1

            blizzardCache.[maxT]

    let isValidCell (pos: Position) =
        pos = startPos || pos = goalPos || isCellFree basin pos

    let queue = Queue<int * Position>()
    let visited = HashSet<int * Position>()

    queue.Enqueue((t0, startPos))
    visited.Add((t0, startPos)) |> ignore

    let rec bfs _ =
        if queue.Count = 0 then
            None
        else
            let (time, pos) = queue.Dequeue()

            if pos = goalPos then
                Some(time, blizzardsAt time)
            else
                let nextTime = time + 1
                let blizz = blizzardsAt nextTime

                candidateMoves pos
                |> List.iter (fun np ->
                    if isValidCell np && isBlizzardFree blizz np then
                        let key = (nextTime, np)

                        if visited.Add(key) then
                            queue.Enqueue((nextTime, np)))

                bfs 0

    bfs 0

let part1 (lines: string list) : string =
    let (basin, start, goal) = lines |> parseInput

    match earliestArrival basin basin.Blizzards 0 start goal with
    | Some t -> t |> fst |> string
    | None -> failwith "error running part 1, no time found"


let part2 (lines: string list) : string =
    let (basin, start, goal) = lines |> parseInput

    let pass1 = Option.get (earliestArrival basin basin.Blizzards 0 start goal)
    let pass2 = Option.get (earliestArrival basin (snd pass1) (fst pass1) goal start)

    match earliestArrival basin (snd pass2) (fst pass2) start goal with
    | Some t -> t |> fst |> string
    | None -> failwith "error running part 2, no time found"

/// <summary>
/// Advent of Code 2022 — Day 23: Unstable Diffusion
/// </summary>
module Aoc2022Lib.Day23

open System

// Types for representing positions and directions
type Position = int * int

type Direction =
    | North
    | South
    | West
    | East

type DirectionSet = Direction list

// Type for representing an Elf's state
type Elf =
    { Position: Position
      ProposedPosition: Position option }

// Type for the grove state
type Grove =
    { Elves: Set<Position>
      Round: int
      DirectionOrder: DirectionSet }

// Direction vectors for movement
let directionVectors =
    function
    | North -> (-1, 0)
    | South -> (1, 0)
    | West -> (0, -1)
    | East -> (0, 1)

// Get all 8 adjacent positions (including diagonals)
let getAdjacentPositions (r: int, c: int) : Position list =
    [ (r - 1, c - 1)
      (r - 1, c)
      (r - 1, c + 1)
      (r, c - 1)
      (r, c + 1)
      (r + 1, c - 1)
      (r + 1, c)
      (r + 1, c + 1) ]

// Get positions to check for a specific direction
let getDirectionPositions (pos: Position) (dir: Direction) : Position list =
    let r, c = pos

    match dir with
    | North -> [ (r - 1, c - 1); (r - 1, c); (r - 1, c + 1) ]
    | South -> [ (r + 1, c - 1); (r + 1, c); (r + 1, c + 1) ]
    | West -> [ (r - 1, c - 1); (r, c - 1); (r + 1, c - 1) ]
    | East -> [ (r - 1, c + 1); (r, c + 1); (r + 1, c + 1) ]

// Parse the input grid to find initial elf positions
let parseInput (lines: string list) : Set<Position> =
    let rec loop (r: int) (acc: Set<Position>) =
        function
        | [] -> acc
        | line :: rest ->
            let newSet =
                line
                |> Seq.mapi (fun c ch -> if ch = '#' then Some(r, c) else None)
                |> Seq.choose id
                |> Set.ofSeq

            loop (r + 1) (Set.union acc newSet) rest

    loop 0 Set.empty lines

// Check if any elves are in the given positions
let hasElvesAt (elves: Set<Position>) (positions: Position list) : bool =
    List.exists (fun elf -> Set.contains elf elves) positions

// Check if an elf should move (has neighbors)
let shouldMove (elfPos: Position) (elves: Set<Position>) : bool =
    let adjacentPositions = getAdjacentPositions (elfPos)
    hasElvesAt elves adjacentPositions

// Propose a move for an elf based on current direction order
let proposeMove (elfPos: Position) (elves: Set<Position>) (directionOrder: DirectionSet) : Position option =

    if not (shouldMove elfPos elves) then
        None
    else
        let rec tryDir (lst: Direction list) =
            match lst with
            | [] -> None
            | hd :: tl ->
                let neighborElves =
                    getDirectionPositions elfPos hd |> List.filter (fun x -> Set.contains x elves)

                if neighborElves.Length = 0 then
                    let dirx, diry = (directionVectors hd)
                    Some(fst elfPos + dirx, snd elfPos + diry)
                else
                    tryDir tl

        tryDir directionOrder


// Get all proposed moves for all elves
let getAllProposedMoves (elves: Set<Position>) (directionOrder: DirectionSet) : Map<Position, Position> =
    elves
    |> Seq.choose (fun elf ->
        match (proposeMove elf elves directionOrder) with
        | Some pos -> Some(elf, pos)
        | None -> None)
    |> Map.ofSeq


// Filter out conflicting moves (multiple elves proposing same destination)
let filterConflictingMoves (proposedMoves: Map<Position, Position>) : Map<Position, Position> =
    proposedMoves
    |> Map.toSeq
    |> Seq.groupBy snd
    |> Seq.filter (fun (_, pairs) -> Seq.length pairs = 1)
    |> Seq.map (fun (_, pairs) -> Seq.head pairs)
    |> Map.ofSeq

// Execute the valid moves
let executeMoves (elves: Set<Position>) (validMoves: Map<Position, Position>) : Set<Position> =
    elves
    |> Set.map (fun elf ->
        match Map.containsKey elf validMoves with
        | true -> validMoves.[elf]
        | false -> elf)

// Rotate the direction order (move first to end)
let rotateDirections (directions: DirectionSet) : DirectionSet =
    List.tail directions @ [ List.head directions ]

// Simulate one round of the diffusion process
let simulateRound (grove: Grove) : Grove =
    let elves = grove.Elves
    let directions = grove.DirectionOrder
    let proposed = getAllProposedMoves elves directions
    let filtered = filterConflictingMoves proposed
    let finalPositions = executeMoves elves filtered

    { Elves = finalPositions
      Round = grove.Round + 1
      DirectionOrder = rotateDirections directions }

// Simulate multiple rounds
let simulateRounds (initialElves: Set<Position>) (numRounds: int) : Set<Position> =
    let initialDirections = [ North; South; West; East ]

    let initialGrove =
        { Elves = initialElves
          Round = 0
          DirectionOrder = initialDirections }

    let finalGrove =
        [ 1..numRounds ] |> List.fold (fun grove _ -> simulateRound grove) initialGrove

    finalGrove.Elves

// Calculate the bounding rectangle of all elves
let getBoundingRectangle (elves: Set<Position>) : (int * int) * (int * int) =
    let minR = elves |> Seq.minBy fst |> fst
    let maxR = elves |> Seq.maxBy fst |> fst
    let minC = elves |> Seq.minBy snd |> snd
    let maxC = elves |> Seq.maxBy snd |> snd
    ((minR, minC), (maxR, maxC))

// Count empty ground tiles in the bounding rectangle
let countEmptyGroundTiles (elves: Set<Position>) : int =
    let boundingBox = getBoundingRectangle elves
    let x = fst (snd boundingBox) - fst (fst boundingBox) + 1
    let y = snd (snd boundingBox) - snd (fst boundingBox) + 1
    x * y - elves.Count

// Part 1: Simulate 10 rounds and count empty ground tiles
let part1 (lines: string list) : string =
    let initialPositions = parseInput lines
    let finalElfPositions = simulateRounds initialPositions 10
    countEmptyGroundTiles finalElfPositions |> string


// Part 2: Find the round where no elf moves (placeholder for future)
let part2 (lines: string list) : string =
    let initialPositions = parseInput lines

    let initialGrove =
        { Elves = initialPositions
          Round = 0
          DirectionOrder = [ North; South; West; East ] }

    let rec loop (grove: Grove) =
        let newGrove = simulateRound grove

        match (grove.Elves = newGrove.Elves) with
        | true -> newGrove.Round
        | false ->
            loop
                { Elves = newGrove.Elves
                  Round = newGrove.Round
                  DirectionOrder = newGrove.DirectionOrder }

    loop initialGrove |> string

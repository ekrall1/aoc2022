/// <summary>
/// Advent of Code 2022 — Day 17
/// </summary>
module Aoc2022Lib.Day17

open System

type Row = Row of int
type Rows = Row array
type Shapes = Rows array
type Moves = string

module Row =
    let create (n: int) =
        if n < 0 || n > 0b1_1111_1111 then failwithf "Row value out of range: %d" n
        Row n
    let value (Row n) = n
    let inline band (Row a) (Row b) = Row (a &&& b)
    let inline bor  (Row a) (Row b) = Row (a ||| b)
    let inline shl  (Row a) k = create ((a <<< k) &&& 0b1_1111_1111)
    let inline shr  (Row a) k = create (a >>> k)

let ShapesRaw = "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"

let buildShapeArray (raw: string) : Shapes =
    let toRow (line: string) =
        line.Trim().ToCharArray()
        |> Array.mapi (fun i c -> if c = '#' then 1 <<< (7 - i) else 0)
        |> Array.sum
        |> fun x -> x >>> 2  // spawn two from the left wall
        |> Row.create
    raw.Replace("\r\n", "\n").Trim().Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun shape ->
        shape.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map toRow
        |> Array.rev) // bottom-first: index 0 is the bottom row

let inline overlapsAt (well: Row array) (shape: Rows) (pos: int) =
    shape
    |> Array.mapi (fun i r -> Row.band r well.[pos + i])
    |> Array.exists (fun (Row n) -> n <> 0)

let tryJet (well: Row array) (shape: Rows) (pos: int) (instr: char) : Rows =
    let shiftL = Array.map (fun r -> Row.shl r 1)
    let shiftR = Array.map (fun r -> Row.shr r 1)
    let cand =
        match instr with
        | '<' -> shiftL shape
        | '>' -> shiftR shape
        | _   -> shape
    if overlapsAt well cand pos then shape else cand

let topIndex (well: Row array) =
    well |> Array.findIndexBack (fun (Row n) -> n <> 0b100000001)

let ensureCapacity (well: Row array) (emptyRow: Row) (t: int) (h: int) =
    let need = t + h + 4
    if well.Length >= need then well
    else Array.append well (Array.init (need - well.Length) (fun _ -> emptyRow))

let mergeAt (well: Row array) (shape: Rows) (pos: int) =
    let slice = well.[pos .. pos + shape.Length - 1]
    for i = 0 to shape.Length - 1 do
        slice.[i] <- Row.bor slice.[i] shape.[i]
    well.[pos .. pos + shape.Length - 1] <- slice

let mkCycler<'a> (arr: 'a array) : (unit -> 'a) =
    let e = Seq.initInfinite (fun i -> arr.[i % arr.Length])
    let en = e.GetEnumerator()
    fun () -> en.MoveNext() |> ignore; en.Current

let fallRock (well: Row array) (shape0: Rows) (nextInstr: unit -> char) (spawnBottom: int) =
    let mutable shape, pos, falling = shape0, spawnBottom, true
    while falling do
        shape <- tryJet well shape pos (nextInstr())
        if overlapsAt well shape (pos - 1) then (mergeAt well shape pos; falling <- false)
        else pos <- pos - 1

let part1 (lines: string list) =
    let shapes = buildShapeArray ShapesRaw
    let emptyRow, floor = Row.create 0b100000001, Row.create 0b111111111
    let mutable well = [| floor |]
    let nextShape = mkCycler shapes
    let nextInstr = mkCycler (lines.[0].ToCharArray())
    for _ in 1 .. 2022 do
        let s = nextShape()
        let t = topIndex well
        well <- ensureCapacity well emptyRow t s.Length
        fallRock well s nextInstr (t + 4)
    topIndex well |> string

let part2 (lines: string list) =
    "Not implemented"
    //let shapes = buildShapeArray ShapesRaw
    //let emptyRow, floor = Row.create 0b100000001, Row.create 0b111111111
    //let mutable well = [| floor |]
    //let nextShape = mkCycler shapes
    //let nextInstr = mkCycler (lines.[0].ToCharArray())
    //// Use a 64-bit integer for the loop counter
    //for _ in 1L .. 1000000000000L do
    //    let s = nextShape()
    //    let t = topIndex well
    //    well <- ensureCapacity well emptyRow t s.Length
    //    fallRock well s nextInstr (t + 4)
    //topIndex well |> string
  

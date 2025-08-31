/// <summary>
/// Advent of Code 2022 — Day 17
/// </summary>
module Aoc2022Lib.Day17

open System

type Row = Row of int
type Rows = Row array
type Shapes = Rows array
type Moves = string

[<CustomEquality; NoComparison>]
type Cursor<'K when 'K: equality> =
    { idx: int
      key: 'K }

    override x.Equals(o: obj) =
        match o with
        | :? Cursor<'K> as y -> x.key = y.key
        | _ -> false

    override x.GetHashCode() = hash x.key

module Row =
    let create (n: int) =
        if n < 0 || n > 0b1_1111_1111 then
            failwithf "Row value out of range: %d" n

        Row n

    let value (Row n) = n
    let inline band (Row a) (Row b) = Row(a &&& b)
    let inline bor (Row a) (Row b) = Row(a ||| b)
    let inline shl (Row a) k = create ((a <<< k) &&& 0b1_1111_1111)
    let inline shr (Row a) k = create (a >>> k)

let ShapesRaw =
    "####

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
        |> fun x -> x >>> 2 // spawn two from the left wall
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
        | _ -> shape

    if overlapsAt well cand pos then shape else cand

let topIndex (well: Row array) =
    well |> Array.findIndexBack (fun (Row n) -> n <> 0b100000001)

let ensureCapacity (well: Row array) (emptyRow: Row) (t: int) (h: int) =
    let need = t + h + 4

    if well.Length >= need then
        well
    else
        Array.append well (Array.init (need - well.Length) (fun _ -> emptyRow))

let mergeAt (well: Row array) (shape: Rows) (pos: int) =
    let slice = well.[pos .. pos + shape.Length - 1]

    for i = 0 to shape.Length - 1 do
        slice.[i] <- Row.bor slice.[i] shape.[i]

    well.[pos .. pos + shape.Length - 1] <- slice

let mkCycler<'a> (arr: 'a array) : (unit -> 'a * int) =
    let mutable i = 0

    fun () ->
        let v, idx = arr.[i], i
        i <- (i + 1) % arr.Length
        v, idx

let getSkyLine (well: Row array) =
    let top = topIndex well
    well.[top]

let fallRock (well: Row array) (shape0: Rows) (nextInstr: unit -> char * int) (spawnBottom: int) =
    let mutable shape, pos, falling = shape0, spawnBottom, true
    let mutable instr: char = ' '
    let mutable lastidx = 0

    while falling do
        let instrNxt = nextInstr ()
        instr <- fst instrNxt
        lastidx <- snd instrNxt
        shape <- tryJet well shape pos instr

        if overlapsAt well shape (pos - 1) then
            (mergeAt well shape pos
             falling <- false)
        else
            pos <- pos - 1

    (getSkyLine well, lastidx)

let floyd (f: 'a -> 'a) (x0: 'a) =
    let mutable tortoise = f x0
    let mutable hare = f (f x0)

    while tortoise <> hare do
        tortoise <- f tortoise
        hare <- f (f hare)

    (tortoise, hare, x0)

let findStart (f: 'a -> 'a) (tortoise: 'a) (hare: 'a) (x0: 'a) =
    let mutable mu = 0
    let mutable tortoise = x0
    let mutable hare = hare

    while tortoise <> hare do
        tortoise <- f tortoise
        hare <- f hare
        mu <- mu + 1

    mu

let floydIterate (f: 'a -> 'a) (x0: 'a) =
    let mutable (tortoise, hare, x0) = floyd f x0
    let mu = findStart f tortoise hare x0

    let mutable lam = 1
    hare <- f tortoise

    while tortoise <> hare do
        hare <- f hare
        lam <- lam + 1

    (mu, lam)

let part1 (lines: string list) =
    let shapes = buildShapeArray ShapesRaw
    let emptyRow, floor = Row.create 0b100000001, Row.create 0b111111111
    let mutable well = [| floor |]
    let nextShape = mkCycler shapes
    let nextInstr = mkCycler (lines.[0].ToCharArray())

    for _ in 1..2022 do
        let s, _ = nextShape ()
        let t = topIndex well
        well <- ensureCapacity well emptyRow t s.Length
        fallRock well s nextInstr (t + 4) |> ignore

    topIndex well |> string

let part2 (lines: string list) =
    let shapes = buildShapeArray ShapesRaw
    let emptyRow, floor = Row.create 0b100000001, Row.create 0b111111111
    let mutable well = [| floor |]
    let nextShape = mkCycler shapes
    let nextInstr = mkCycler (lines.[0].ToCharArray())
    let mutable keys = []
    let mutable heights = []

    for _ in 1..10000 do
        let s, sIdx = nextShape ()
        let t = topIndex well
        well <- ensureCapacity well emptyRow t s.Length
        let topline, iIdx = fallRock well s nextInstr (t + 4)
        let key = (topline, sIdx, iIdx)
        let hAfter = topIndex well
        keys <- keys @ [ key ]
        heights <- heights @ [ hAfter ]

    let arr = keys |> List.toArray
    let heightAfter k = if k <= 0 then 0 else heights.[k - 1]

    let step (c: Cursor<_>) =
        let newIdx = (c.idx + 1) % arr.Length
        { idx = newIdx; key = arr.[newIdx] }

    let x0 = { idx = 0; key = arr.[0] }

    let (mu, lam) = floydIterate step x0

    let pre = heightAfter mu
    let remaining = 1_000_000_000_000L - (int64 mu)
    let cycles = remaining / (int64 lam)
    let remainder = int (remaining % (int64 lam))

    let remIncrease =
        if remainder = 0 then
            0L
        else
            int64 (heightAfter (mu + remainder) - heightAfter mu)

    let increase = int64 (heightAfter (mu + lam) - pre)

    (int64 pre + (cycles * increase) + remIncrease) |> string

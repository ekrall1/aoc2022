/// <summary>
/// Advent of Code 2022 — Day 20
/// </summary>
module Aoc2022Lib.Day20

open System

let makeInitialList (lines: string list) =
    let rec formList (cur: string list) (acc: int64 list) =
        match cur with
        | [] -> acc
        | hd :: tl -> formList tl ((Int64.Parse hd) :: acc)

    formList lines [] |> List.rev

let inline posMod64 (x: int64) (m: int64) =
    let r = x % m
    if r < 0L then r + m else r

let private removeItem (items: ResizeArray<int * int64>) (atPos: int array) (idx: int) =
    let item = items.[idx]
    items.RemoveAt(idx)

    for k = idx to items.Count - 1 do
        let (j, v) = items.[k]
        atPos.[j] <- k

    item

let private addItemAt (items: ResizeArray<int * int64>) (atPos: int array) (item: int * int64) (idx: int) =
    items.Insert(idx, item)

    for k = idx to items.Count - 1 do
        let (j, v) = items.[k]
        atPos.[j] <- k

let private getDeterminant (zeroIdx: int) (offset: int) (items: ResizeArray<int * int64>) =
    let L = items.Count
    let idx = (zeroIdx + offset % L) % L
    items.[idx]

let decryptPart1 (lst: int64 list) =

    let length = lst.Length
    let items = lst |> List.mapi (fun i v -> (i, v)) |> ResizeArray
    let atPos = Array.init length id
    let mutable zeroIndex = 0

    for idx = 0 to length - 1 do
        let i = atPos.[idx]
        let (j, v) = removeItem items atPos i
        let item = (j, v)
        let target = (i + int (posMod64 v (int64 (length - 1)))) % (length - 1)
        addItemAt items atPos item target
        zeroIndex <- Seq.findIndex (fun (_, v) -> v = 0L) items

    let (_, det1) = getDeterminant zeroIndex 1000 items
    let (_, det2) = getDeterminant zeroIndex 2000 items
    let (_, det3) = getDeterminant zeroIndex 3000 items
    det1 + det2 + det3


let part1 (lines: string list) =
    let initialList = makeInitialList lines
    decryptPart1 initialList |> string

let part2 (lines: string list) = "Not implemented"

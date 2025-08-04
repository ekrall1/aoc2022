module Aoc2022Lib.Day13

open System.Text.Json

type Packet =
    | PInt of int
    | PList of Packet list

let rec ofJson (el: JsonElement) : Packet =
    match el.ValueKind with
    | JsonValueKind.Number -> PInt (el.GetInt32())
    | JsonValueKind.Array ->
        el.EnumerateArray()
        |> Seq.map ofJson
        |> Seq.toList
        |> PList
    | _ -> failwith "Invalid packet"

let parseLine (line: string) : Packet =
    use doc = JsonDocument.Parse(line)
    ofJson doc.RootElement

let rec private comparePackets (left: Packet) (right: Packet) : int =
    match left, right with
    | PInt a, PInt b -> compare a b
    | PList l, PList r ->
        let rec loop l r =
            match l, r with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | lh :: lt, rh :: rt ->
                match comparePackets lh rh with
                | 0 -> loop lt rt
                | c -> c
        loop l r
    | PInt a, PList r -> comparePackets (PList [ PInt a ]) (PList r)
    | PList l, PInt b -> comparePackets (PList l) (PList [ PInt b ])

let part1 (lines: string list) : string =
    let rec toPairs acc remaining =
        match remaining with
        | [] -> List.rev acc
        | l1 :: l2 :: rest ->
            let pair = parseLine l1, parseLine l2
            let rest' =
                match rest with
                | "" :: tail -> tail
                | _ -> rest
            toPairs (pair :: acc) rest'
        | _ -> failwith "Malformed input"

    toPairs [] lines
    |> List.mapi (fun idx (l, r) -> if comparePackets l r < 0 then idx + 1 else 0)
    |> List.sum
    |> string

let part2 (lines: string list) : string =
    let packets =
        lines
        |> List.filter (fun line -> line.Trim() <> "")
        |> List.map parseLine

    let divider1 = PList [ PList [ PInt 2 ] ]
    let divider2 = PList [ PList [ PInt 6 ] ]

    // Add divider packets
    let allPackets = packets @ [ divider1; divider2 ]

    let sorted =
        allPackets
        |> List.sortWith comparePackets

    let pos1 = sorted |> List.findIndex ((=) divider1) |> (+) 1
    let pos2 = sorted |> List.findIndex ((=) divider2) |> (+) 1

    // Return the product as string
    (pos1 * pos2).ToString()
module Aoc2022Lib.Day16

open System.Text.RegularExpressions
open Aoc2022Lib.Railway
open System.Collections.Generic

type InputList<'T> = ('T * 'T list) list
type InputTable<'V, 'W when 'V: comparison> = Map<'V, Map<'V, 'W>>

type ParseError =
    | InvalidLine of string
    | EmptyInput

type SolutionError =
    | ParseError of ParseError
    | InvalidFlowRate


let parseLine (line: string) : Result<(string) * string list, ParseError> =
    if System.String.IsNullOrWhiteSpace(line) then
        Error(InvalidLine line)
    else
        let pattern = @"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
        let m = Regex.Match(line, pattern)

        if m.Success then
            let valve = m.Groups.[1].Value
            let leadsTo = m.Groups.[3].Value.Split(", ") |> List.ofArray
            Ok(valve, leadsTo)
        else
            Error(InvalidLine line)

let singleFlowRate (line: string) : Result<(string * int), ParseError> =
    if System.String.IsNullOrWhiteSpace(line) then
        Error(InvalidLine line)
    else
        let pattern = @"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
        let m = Regex.Match(line, pattern)

        if m.Success then
            let valve = m.Groups.[1].Value
            let flowRateStr = m.Groups.[2].Value

            match System.Int32.TryParse(flowRateStr) with
            | (true, flowRate) -> Ok(valve, flowRate)
            | _ -> Error(InvalidLine line)
        else
            Error(InvalidLine line)


let buildInputList (input: string list) : Result<InputList<string>, ParseError> =
    if List.isEmpty input then
        Error EmptyInput
    else
        (Ok []: Result<InputList<string>, ParseError>)
        |> fun acc ->
            (acc, input)
            ||> List.fold (fun acc line -> acc >>= fun lst -> parseLine line <!> fun v -> v :: lst)
            <!> List.rev

let neighborsOf (v: string) (graph: InputList<string>) : string list =
    graph
    |> List.tryFind (fun (x, _) -> x = v)
    |> Option.map snd
    |> Option.defaultValue []

let bfs (start: string) (graph: InputList<string>) : Map<string, int> =
    let queue = Queue<string * int>()
    let mutable visited = Set.empty<string>
    let mutable dist = Map.empty<string, int>

    queue.Enqueue(start, 0)

    while queue.Count > 0 do
        let (u, d) = queue.Dequeue()

        if not (visited.Contains u) then
            visited <- visited.Add u

            for v in neighborsOf u graph do
                if not (visited.Contains v) then
                    dist <- dist.Add(v, d + 1)
                    queue.Enqueue(v, d + 1)

    dist

let dfs (start: string) (graph: InputTable<string, int>) (flowRates: Map<string, int>) (timeLimit: int) =
    let useful =
        flowRates
        |> Seq.choose (fun (KeyValue(k, r)) -> if r > 0 then Some k else None)
        |> Set.ofSeq
        |> fun s -> s.Add start // always allow starting valve
    // Build an index for valves so we can use bit operations
    let indexOf = Dictionary<string, int>()
    let mutable nextIdx = 0

    for KeyValue(k, r) in flowRates do
        if r > 0 then
            indexOf.[k] <- nextIdx
            nextIdx <- nextIdx + 1

    let inline hasBit (mask: int) (i: int) = (mask &&& (1 <<< i)) <> 0
    let inline setBit (mask: int) (i: int) = mask ||| (1 <<< i)

    let bestByMask = Array.zeroCreate (1 <<< nextIdx)

    let memo = Dictionary<struct (string * int * int), int>()
    let stack = Stack<string * int * int * int>()

    stack.Push(start, 0, 0, 0)

    while stack.Count > 0 do
        let (u, mask, t, released) = stack.Pop()

        if t > timeLimit then
            ()
        else
            if released > bestByMask.[mask] then
                bestByMask.[mask] <- released

            let key = struct (u, mask, t)

            match memo.TryGetValue key with
            | true, prev when prev >= released -> () // prune
            | _ ->
                memo.[key] <- released

                for KeyValue(v, d) in graph.[u] do
                    match Map.tryFind v flowRates with
                    | Some rate when rate > 0 && indexOf.ContainsKey v ->
                        let idx = indexOf.[v]

                        if not (hasBit mask idx) then
                            let cost = d + 1 // move and open
                            let t' = t + cost

                            if t' < timeLimit then
                                let gain = rate * (timeLimit - t')
                                let mask' = setBit mask idx
                                stack.Push(v, mask', t', released + gain)
                    | _ -> ()

    bestByMask, nextIdx

let parseFlowRates (input: string list) : Result<Map<string, int>, ParseError> =
    input
    |> List.filter (fun line -> not (System.String.IsNullOrWhiteSpace(line)))
    |> List.map singleFlowRate
    |> List.fold
        (fun acc result -> acc >>= fun rates -> result >>= fun (valve, rate) -> Ok(rates.Add(valve, rate)))
        (Ok Map.empty)

let parseInput (input: string list) : Result<InputTable<string, int>, ParseError> =
    buildInputList input
    <!> fun graph -> graph |> List.map (fun (v, _) -> (v, bfs v graph)) |> Map.ofList

let part1 (lines: string list) =
    let flowRates =
        match lines |> parseFlowRates with
        | Ok fr -> fr
        | Error e -> failwithf "Failed to parse flow rates: %A" e

    let distTable =
        match lines |> parseInput with
        | Ok dt -> dt
        | Error e -> failwithf "Failed to parse input: %A" e

    let bestByMask, _ = dfs "AA" distTable flowRates 30
    (Array.max bestByMask) |> string


let part2 (lines: string list) =
    let flowRates =
        match lines |> parseFlowRates with
        | Ok fr -> fr
        | Error e -> failwithf "Failed to parse input %A" e

    let distTable =
        match lines |> parseInput with
        | Ok dt -> dt
        | Error e -> failwithf "Failed to parse input: %A" e

    let bestByMask, bits = dfs "AA" distTable flowRates 26
    let fullMask = (1 <<< bits) - 1

    for i = 0 to bits - 1 do
        for m = 0 to fullMask do
            if (m &&& (1 <<< i)) <> 0 then
                let sub = m ^^^ (1 <<< i)

                if bestByMask.[m] < bestByMask.[sub] then
                    bestByMask.[m] <- bestByMask.[sub]

    let mutable best = 0

    for m in 0..fullMask do
        let s = bestByMask.[m] + bestByMask.[fullMask ^^^ m]

        if s > best then
            best <- s

    best |> string

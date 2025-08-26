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
    let memo = Dictionary<(string * Set<string> * int), int>()
    let stack = Stack<string * Set<string> * int * int>()
    let mutable best = 0

    stack.Push(start, Set.empty, 0, 0)

    while stack.Count > 0 do
        let (u, opened, t, released) = stack.Pop()

        if t > timeLimit then
            ()
        else
            if released > best then
                best <- released

            let key = (u, opened, t)

            match memo.TryGetValue key with
            | true, prev when prev >= released -> () // prune
            | _ ->
                memo.[key] <- released

                if t < timeLimit then
                    let tNext = t + 1

                    // Option 1: open current valve (if beneficial and not opened yet)
                    match Map.tryFind u flowRates with
                    | Some rate when rate > 0 && not (opened.Contains u) && tNext <= timeLimit ->
                        let gain = rate * (timeLimit - tNext)
                        stack.Push(u, opened.Add u, tNext, released + gain)
                    | _ -> ()

                    // Option 2: move to neighbors
                    if tNext <= timeLimit then
                        for v in graph[u] do
                            stack.Push(v.Key, opened, t + v.Value, released)

    best





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

    dfs "AA" distTable flowRates 30 |> string

let part2 (lines: string list) = "not implemented"

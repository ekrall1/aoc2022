/// <summary>
/// Advent of Code 2022 — Day 16 helper library (graph parsing, distance table, and solvers).
/// </summary>
/// <remarks>
/// Performance notes:
/// <list type="bullet">
///   <item><description>BFS per source is O(V+E); all-pairs via BFS is O(V·(V+E)).</description></item>
///   <item><description>DFS/DP explores masks over K positive-rate valves: ~O(2^K · K) transitions.</description></item>
/// </list>
/// </remarks>
module Aoc2022Lib.Day16

open System.Text.RegularExpressions
open Aoc2022Lib.Railway
open System.Collections.Generic

/// <summary>
/// Adjacency list where each entry is (vertex, neighbors).
/// </summary>
/// <typeparam name="T">Vertex identifier type.</typeparam>
/// <example>
/// <code lang="fsharp">
/// let g : InputList&lt;string&gt; = [ "AA", ["BB"; "CC"]; "BB", ["CC"] ]
/// </code>
/// </example>
type InputList<'T> = ('T * 'T list) list

/// <summary>
/// Table of pairwise distances, i.e., a map of source → (target → weight).
/// </summary>
/// <typeparam name="V">Vertex type (must support comparison for map keys).</typeparam>
/// <typeparam name="W">Edge weight type (e.g., hop count as int).</typeparam>
/// <remarks>Used as an all-pairs shortest-path table with hop counts.</remarks>
type InputTable<'V, 'W when 'V: comparison> = Map<'V, Map<'V, 'W>>

// ---------------- parsing ----------------

/// <summary>
/// Precompiled regex for lines: <c>Valve XX has flow rate=NN; tunnels lead to valves A, B, ...</c>.
/// </summary>
/// <remarks>
/// Group 1: valve name (two chars). Group 2: flow (digits). Group 3: CSV neighbors.
/// Regex is anchored and compiled for speed.
/// </remarks>
let private rxLine =
    Regex(@"^Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)$", RegexOptions.Compiled)

/// <summary>Parsing errors for input lines.</summary>
type ParseError =
    /// <summary>Line was empty/whitespace or did not match the expected format.</summary>
    | InvalidLine of string
    /// <summary>No input lines were provided.</summary>
    | EmptyInput

/// <summary>Domain-level errors (not fully used here but reserved for extensions).</summary>
type SolutionError =
    /// <summary>Wraps a parsing error.</summary>
    | ParseError of ParseError
    /// <summary>Flow rate constraints violated.</summary>
    | InvalidFlowRate

/// <summary>
/// Parse a single line into (valve, neighbors).
/// </summary>
/// <param name="line">Raw input line.</param>
/// <returns><c>Ok(name, neighbors)</c> or <c>Error InvalidLine</c>.</returns>
/// <remarks>Flow is parsed elsewhere when needed; this function only returns neighbors.</remarks>
/// <example>
/// <code lang="fsharp">
/// parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
/// // Ok ("AA", ["DD"; "II"; "BB"])
/// </code>
/// </example>
let parseLine (line: string) : Result<string * string list, ParseError> =
    if System.String.IsNullOrWhiteSpace line then
        Error(InvalidLine line)
    else
        let m = rxLine.Match line

        if not m.Success then
            Error(InvalidLine line)
        else
            Ok(m.Groups.[1].Value, m.Groups.[3].Value.Split(", ") |> Array.toList)

/// <summary>
/// Parse a single line into (valve, flowRate).
/// </summary>
/// <param name="line">Raw input line.</param>
/// <returns><c>Ok(name, flow)</c> or <c>Error InvalidLine</c>.</returns>
/// <example>
/// <code lang="fsharp">
/// singleFlowRate "Valve BB has flow rate=13; tunnels lead to valves CC"
/// // Ok ("BB", 13)
/// </code>
/// </example>
let singleFlowRate (line: string) : Result<string * int, ParseError> =
    if System.String.IsNullOrWhiteSpace line then
        Error(InvalidLine line)
    else
        let m = rxLine.Match line

        if not m.Success then
            Error(InvalidLine line)
        else
            match System.Int32.TryParse m.Groups.[2].Value with
            | true, n -> Ok(m.Groups.[1].Value, n)
            | _ -> Error(InvalidLine line)

/// <summary>
/// Build an adjacency list from raw lines.
/// </summary>
/// <param name="input">All input lines.</param>
/// <returns><c>Ok(InputList)</c> or <c>Error EmptyInput/InvalidLine</c>.</returns>
/// <remarks>Short-circuits on first parse error using Railway operators (<c>&gt;&gt;=</c>, <c>&lt;!&gt;</c>).</remarks>
let buildInputList (input: string list) : Result<InputList<string>, ParseError> =
    if List.isEmpty input then
        Error EmptyInput
    else
        (Ok []: Result<InputList<string>, ParseError>)
        |> fun acc -> (acc, input)
        ||> List.fold (fun acc line -> acc >>= fun lst -> parseLine line <!> fun v -> v :: lst)
        <!> List.rev

// ---------------- graph utils ----------------

/// <summary>
/// Get neighbor list for a vertex from an adjacency list.
/// </summary>
/// <param name="v">Vertex name.</param>
/// <param name="g">Adjacency list.</param>
/// <returns>Neighbor names or empty list if not found.</returns>
let neighborsOf (v: string) (g: InputList<string>) : string list =
    g
    |> List.tryFind (fun (x, _) -> x = v)
    |> Option.map snd
    |> Option.defaultValue []

/// <summary>
/// Breadth-first search (BFS) distances from a start node in an unweighted graph.
/// </summary>
/// <param name="start">Source vertex.</param>
/// <param name="g">Adjacency list.</param>
/// <returns>Map of target → hop distance (start not included unless discovered).</returns>
/// <remarks>
/// Complexity: O(V+E). Distances are in hops (each edge weighs 1).
/// </remarks>
/// <example>
/// <code lang="fsharp">
/// let d = bfs "AA" [ "AA", ["BB"]; "BB", ["CC"] ]
/// // d.["BB"] = 1; d.["CC"] = 2
/// </code>
/// </example>
let bfs (start: string) (g: InputList<string>) : Map<string, int> =
    let rec loop q seen dist =
        match q with
        | [] -> dist
        | (u, d) :: qs ->
            if Set.contains u seen then
                loop qs seen dist
            else
                let ns = neighborsOf u g

                let dist' =
                    ns
                    |> List.fold (fun m v -> if Map.containsKey v m then m else m.Add(v, d + 1)) dist

                loop (qs @ (ns |> List.map (fun v -> v, d + 1))) (Set.add u seen) dist'

    loop [ start, 0 ] Set.empty Map.empty

/// <summary>
/// Parse all flow rates from raw lines.
/// </summary>
/// <param name="input">All input lines.</param>
/// <returns>Map of valve → flow or <c>Error</c> from the first bad line.</returns>
let parseFlowRates (input: string list) : Result<Map<string, int>, ParseError> =
    input
    |> List.filter (System.String.IsNullOrWhiteSpace >> not)
    |> List.map singleFlowRate
    |> List.fold (fun acc r -> acc >>= fun m -> r <!> fun (k, v) -> m.Add(k, v)) (Ok Map.empty)

/// <summary>
/// Parse and build the all-pairs hop distance table.
/// </summary>
/// <param name="input">All input lines.</param>
/// <returns>Map of source → (target → hops).</returns>
/// <remarks>Runs <see cref="bfs"/> from each vertex in the adjacency list.</remarks>
let parseInput (input: string list) : Result<InputTable<string, int>, ParseError> =
    buildInputList input
    <!> fun gl -> gl |> List.map (fun (v, _) -> v, bfs v gl) |> Map.ofList

// ---------------- bitmask helpers ----------------

/// <summary>
/// Assign a stable bit index to each positive-rate valve.
/// </summary>
/// <param name="flow">Valve → flow map.</param>
/// <returns>(index map, number of indexed valves).</returns>
/// <remarks>
/// Only valves with <c>flow &gt; 0</c> receive a bit. This shrinks the DP state space.
/// </remarks>
let buildIndex (flow: Map<string, int>) : Map<string, int> * int =
    flow
    |> Seq.filter (fun (KeyValue(_, r)) -> r > 0)
    |> Seq.mapi (fun i (KeyValue(k, _)) -> k, i)
    |> Map.ofSeq
    |> fun idx -> idx, idx.Count

/// <summary>Test whether a bit is set in a mask.</summary>
/// <param name="m">Mask.</param>
/// <param name="i">Bit index.</param>
/// <returns><c>true</c> if set; otherwise <c>false</c>.</returns>
let inline hasBit (m: int) i = (m &&& (1 <<< i)) <> 0

/// <summary>Set a bit in a mask.</summary>
/// <param name="m">Mask.</param>
/// <param name="i">Bit index.</param>
/// <returns>Mask with bit <paramref name="i"/> set.</returns>
let inline setBit (m: int) i = m ||| (1 <<< i)

// ---------------- search (build bestByMask) ----------------

/// <summary>
/// Depth-first exploration that records the best achievable pressure per mask
/// (i.e., per subset of opened positive-rate valves).
/// </summary>
/// <param name="start">Start valve (usually "AA").</param>
/// <param name="graph">All-pairs hop distances.</param>
/// <param name="flow">Valve → flow map.</param>
/// <param name="timeLimit">Time budget in minutes.</param>
/// <returns>
/// <para><c>bestByMask</c>: array indexed by mask, where each entry is the max pressure
/// achievable by opening exactly the valves in that mask (before subset-closure).</para>
/// <para><c>bits</c>: number of positive-rate valves (mask width).</para>
/// </returns>
/// <remarks>
/// Transition: move from <c>u</c> to <c>v</c> in <c>d</c> hops then open (<c>+1</c>),
/// total cost <c>d+1</c>. Gain <c>rate[v]*(timeLimit - (t + d + 1))</c> if nonnegative.
/// </remarks>
let dfs (start: string) (graph: InputTable<string, int>) (flow: Map<string, int>) (timeLimit: int) =
    let indexOf, bits = buildIndex flow
    let best = Array.zeroCreate (1 <<< bits)

    let rec go u mask t released =
        if released > best.[mask] then
            best.[mask] <- released

        graph.[u]
        |> Seq.iter (fun (KeyValue(v, d)) ->
            match Map.tryFind v flow with
            | Some rate when rate > 0 && Map.containsKey v indexOf ->
                let i = indexOf.[v]

                if hasBit mask i then
                    ()
                else
                    let t' = t + d + 1

                    if t' <= timeLimit then
                        go v (setBit mask i) t' (released + rate * (timeLimit - t'))
            | _ -> ())

    go start 0 0 0
    best, bits

// ---------------- subset closure + combine ----------------

/// <summary>
/// Perform subset-closure so that each mask M stores the best value over all submasks S ⊆ M.
/// </summary>
/// <param name="best">Array of best values per mask (in/out).</param>
/// <param name="bits">Number of mask bits.</param>
/// <returns>The same array instance after in-place closure.</returns>
/// <remarks>
/// Standard SOS DP: for each bit i, if mask has i set, let it inherit value of mask with i cleared.
/// Complexity: O(bits · 2^bits).
/// </remarks>
let closeDown (best: int[]) (bits: int) =
    let full = (1 <<< bits) - 1

    for i in 0 .. bits - 1 do
        for m in 0..full do
            if (m &&& (1 <<< i)) <> 0 then
                let sub = m ^^^ (1 <<< i)

                if best.[m] < best.[sub] then
                    best.[m] <- best.[sub]

    best

/// <summary>
/// Combine two disjoint agent assignments by splitting masks with complements.
/// </summary>
/// <param name="best">Closed array of best-by-mask values.</param>
/// <param name="bits">Number of mask bits.</param>
/// <returns>Maximum combined pressure for two agents working in parallel.</returns>
/// <remarks>
/// For each subset m, the elephant takes fullMask ^^^ m. Both are disjoint.
/// Requires <see cref="closeDown"/> to have been applied first.
/// </remarks>
let splitTwo (best: int[]) (bits: int) =
    let full = (1 <<< bits) - 1
    let mutable ans = 0

    for m in 0..full do
        let s = best.[m] + best.[full ^^^ m]

        if s > ans then
            ans <- s

    ans

// ---------------- parts ----------------

/// <summary>
/// Solve Part 1: single agent, 30 minutes.
/// </summary>
/// <param name="lines">Raw input lines.</param>
/// <returns>Answer as a string (max pressure).</returns>
/// <example>
/// <code lang="fsharp">
/// part1 sampleLines |> printfn "%s"
/// </code>
/// </example>
let part1 (lines: string list) =
    let flow =
        match parseFlowRates lines with
        | Ok x -> x
        | Error e -> failwithf "%A" e

    let dt =
        match parseInput lines with
        | Ok x -> x
        | Error e -> failwithf "%A" e

    let best, _ = dfs "AA" dt flow 30
    (Array.max best) |> string

/// <summary>
/// Solve Part 2: two agents (you + elephant), 26 minutes.
/// </summary>
/// <param name="lines">Raw input lines.</param>
/// <returns>Answer as a string (max combined pressure).</returns>
/// <remarks>
/// Steps:
/// <list type="number">
///   <item><description>Run <see cref="dfs"/> once to fill best-by-mask (exact-open set).</description></item>
///   <item><description>Apply <see cref="closeDown"/> to allow any subset within a mask.</description></item>
///   <item><description>Use <see cref="splitTwo"/> to combine disjoint complements.</description></item>
/// </list>
/// </remarks>
let part2 (lines: string list) =
    let flow =
        match parseFlowRates lines with
        | Ok x -> x
        | Error e -> failwithf "%A" e

    let dt =
        match parseInput lines with
        | Ok x -> x
        | Error e -> failwithf "%A" e

    let mutable best, bits = dfs "AA" dt flow 26
    best <- closeDown best bits
    splitTwo best bits |> string

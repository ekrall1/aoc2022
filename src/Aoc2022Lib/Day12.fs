module Aoc2022Lib.Day12

open System

type Graph = { Nodes: Map<int * int, char> }

let Elevation =
    Map.ofList
        [ ('a', 0)
          ('S', 0)
          ('b', 1)
          ('c', 2)
          ('d', 3)
          ('e', 4)
          ('f', 5)
          ('g', 6)
          ('h', 7)
          ('i', 8)
          ('j', 9)
          ('k', 10)
          ('l', 11)
          ('m', 12)
          ('n', 13)
          ('o', 14)
          ('p', 15)
          ('q', 16)
          ('r', 17)
          ('s', 18)
          ('t', 19)
          ('u', 20)
          ('v', 21)
          ('w', 22)
          ('x', 23)
          ('y', 24)
          ('z', 25)
          ('E', 25) ]

let createGraph (input: string list) =
    let nodes =
        input
        |> List.mapi (fun y line -> line |> Seq.mapi (fun x c -> ((x, y), c)))
        |> Seq.concat
        |> Map.ofSeq

    { Nodes = nodes }

let getStart (graph: Graph) =
    graph.Nodes |> Seq.find (fun kvp -> kvp.Value = 'S') |> (fun kvp -> kvp.Key)

let getEnd (graph: Graph) =
    graph.Nodes |> Seq.find (fun kvp -> kvp.Value = 'E') |> (fun kvp -> kvp.Key)

let getNeighbors (coord: int * int) (graph: Graph) (currentHeight: int) : (int * int) list =
    // get neighboring coordinates in ordinal directions
    let (x, y) = coord

    let neighbors =
        [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
        |> List.filter (fun (nx, ny) -> graph.Nodes.ContainsKey((nx, ny)))
        |> List.filter (fun (nx, ny) ->
            let neighborHeight = Elevation.[graph.Nodes[(nx, ny)]]
            neighborHeight <= currentHeight + 1)

    neighbors

let dijkstra (graph: Graph) (start: int * int) =

    let dist = System.Collections.Generic.Dictionary<int * int, int>()
    let prev = System.Collections.Generic.Dictionary<int * int, option<int * int>>()

    dist[start] <- 0

    let queue = Collections.Generic.PriorityQueue<(int * int), int>()
    queue.Enqueue(start, dist[start])

    while queue.Count > 0 do
        let current = queue.Dequeue()
        let currentDist = dist[current]

        for neighbor in getNeighbors current graph Elevation.[graph.Nodes[current]] do
            let newDist = currentDist + 1

            if not (dist.ContainsKey(neighbor)) || newDist < dist[neighbor] then
                dist[neighbor] <- newDist
                prev[neighbor] <- Some current
                queue.Enqueue(neighbor, newDist)

    dist, prev

let part1 (lines: string list) : string =
    let graph = createGraph lines
    let startPos = getStart graph
    let endPos = getEnd graph
    let dist, _ = dijkstra graph startPos

    match dist.TryGetValue endPos with
    | (true, d) -> d.ToString()
    | (false, _) -> "No path found"

let part2 (lines: string list) : string =
    let graph = createGraph lines
    let endPos = getEnd graph
    let mutable bestDist = System.Int32.MaxValue

    let aPositions =
        graph.Nodes
        |> Seq.filter (fun kvp -> kvp.Value = 'a')
        |> Seq.map (fun kvp -> kvp.Key)
        |> Seq.toList

    for pos in aPositions do
        let dist, _ = dijkstra graph pos

        match dist.TryGetValue endPos with
        | (true, d) when d < bestDist -> bestDist <- d
        | _ -> ()

    bestDist.ToString()

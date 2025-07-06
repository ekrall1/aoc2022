namespace Aoc2022Lib

open System.Collections.Generic
open System.Text.RegularExpressions

module Day07 =
    (** Part 1 - build a graph and find directory sizes, find all directories <= 100000 *)

    type FileSysElement = { name: string; size: int64 }

    type Directory =
        { name: string
          parentDirectory: string
          elements: FileSysElement list }


    let BFSPart1 (start: string) (graph: Dictionary<string, Directory>) =

        let mutable sumDirs = 0L

        let rec search (stack: string list) (visited: Set<string>) =
            match stack with
            | [] -> sumDirs
            | hd :: tl ->
                if Set.contains hd visited then
                    search tl visited
                else
                    let visited' = visited.Add hd
                    let curElements = graph.[hd].elements
                    let mutable remainingDirs: string list = tl

                    for f in curElements do
                        if f.size <> 0 then
                            sumDirs <- sumDirs + f.size
                        else
                            remainingDirs <- remainingDirs @ [ f.name ]

                    search remainingDirs visited'

        search [ start ] Set.empty

    let fullPath parent child =
        if child = "/" then "/"                       // jump to root
        elif parent = "/" then "/" + child
        else parent + "/" + child

    let InitializeGraph (input: string list) =

        let rec executeCommands (lst: string list) (currentLevel: string) (graph: Dictionary<string, Directory>) =
            match lst with
            | [] -> graph
            | hd :: tl ->
                if hd.StartsWith "$ cd .." then
                    executeCommands tl graph[currentLevel].parentDirectory graph
                else if hd.StartsWith "$ cd" then
                    // "$ cd foo" → dirName = "foo"
                    let dirName =
                        hd.Split(' ', System.StringSplitOptions.RemoveEmptyEntries).[2]
                    let dirPath  = fullPath currentLevel dirName 

                    // ── ensure the target directory exists ──────────────────────────────
                    if graph.ContainsKey dirPath |> not then
                        graph.[dirPath] <-{ 
                            name            = dirPath
                            parentDirectory = currentLevel
                            elements        = [] 
                        }

                    // ── add a reference to it in the current directory (if absent) ─────
                    let curDir = graph.[currentLevel]
                    let dirRef = { name = dirPath; size = 0 }

                    if not (List.contains dirRef curDir.elements) then
                        graph.[currentLevel] <-
                            { curDir with elements = dirRef :: curDir.elements }

                    // ── recurse one level down ─────────────────────────────────────────
                    executeCommands tl dirPath graph
                else
                    if hd.StartsWith "dir " then
                        let dirName = hd.Split(' ')[1]
                        let dirPath = fullPath currentLevel dirName

                        // create the child dir node if brand-new
                        if graph.ContainsKey dirPath |> not then
                            graph.[dirPath] <-{ 
                                name = dirPath
                                parentDirectory = currentLevel
                                elements = [] 
                            }

                        // add a reference to it in the current directory
                        let curDir = graph.[currentLevel]
                        let dirRef = { name = dirPath; size = 0 }

                        if List.contains dirRef curDir.elements |> not then
                            graph.[currentLevel] <-
                                { curDir with elements = dirRef :: curDir.elements }

                    else
                        let m = Regex.Match(hd, @"^[0-9]")

                        if m.Success then
                            let parts = hd.Split(" ")
                            let size: int64 = System.Int64.Parse parts[0]
                            let newFile = { name = parts[1]; size = size }
                            let currentDir = graph.[currentLevel]

                            graph[currentLevel] <-
                                { currentDir with
                                    elements = currentDir.elements @ [ newFile ] }

                    executeCommands tl currentLevel graph

        let graph: Dictionary<string, Directory> = Dictionary()
        graph["/"] <- { name = "/"; parentDirectory = "/"; elements = [] }

        executeCommands input "/" graph

    let part1 input =
        let graph = InitializeGraph input
        let mutable finalSum = 0L
        for key in graph.Keys do
            let levelSum = BFSPart1 key graph
            if levelSum <= 100000L then finalSum <- finalSum + levelSum
        finalSum |> string

    let part2 input =
        let graph = InitializeGraph input
        let totalDiskSpace = 70000000L
        let unusedDiskSpaceTarget = 30000000L
        let outerDirectorySize = BFSPart1 "/" graph
        let targetRemoveSize = unusedDiskSpaceTarget - (totalDiskSpace - outerDirectorySize)
        let mutable smallest = System.Int64.MaxValue

        for key in graph.Keys do
            let levelSum = BFSPart1 key graph
            if levelSum >= targetRemoveSize && levelSum < smallest then
                smallest <- levelSum

        smallest |> string

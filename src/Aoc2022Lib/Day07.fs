namespace Aoc2022Lib

open System.Text.RegularExpressions

module Day07 =

    // ────────────────────────────── types ────────────────────────────────

    type FileSysElement = { name: string; size: int64 }

    type Directory =
        { name: string
          parentDirectory: string
          elements: FileSysElement list }

    type Graph = Map<string, Directory>

    // ───────────────────── railway helpers (Result<'T, string>) ──────────

    module R =
        let inline (>>=) r f = Result.bind f r
        let inline (<!>) r f = Result.map f r

    open R

    // ──────────────────────── parsing input ───────────────────────────

    type Cmd =
        | CdRoot
        | CdUp
        | CdInto of string // dir name
        | DirListing of string // "dir xyz"
        | FileListing of string * int64 // "123 foo"
        | NoOp // lines we can ignore (e.g. "$ ls")

    let (|Regex|_|) (pat: string) (input: string) =
        let m = Regex.Match(input, pat)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseLine =
        function
        | "$ cd /" -> Ok CdRoot
        | "$ cd .." -> Ok CdUp
        | Regex "^\$ cd (.+)$" [ d ] -> Ok(CdInto d)
        | "$ ls" -> Ok NoOp
        | Regex "^dir (.+)$" [ d ] -> Ok(DirListing d)
        | Regex "^(\d+) (.+)$" [ s; n ] -> Ok(FileListing(n, int64 s))
        | other -> Error($"Unrecognised line: {other}")

    // ─────────────────── helpers ───────────────

    let fullPath parent child =
        if child = "/" then "/"
        elif parent = "/" then "/" + child
        else parent + "/" + child

    let ensureDir path parent (g: Graph) : Graph =
        match g |> Map.tryFind path with
        | Some _ -> g
        | None ->
            g
            |> Map.add
                path
                { name = path
                  parentDirectory = parent
                  elements = [] }

    let addElem dirPath elem (g: Graph) : Graph =
        g
        |> Map.change dirPath (function
            | None -> None
            | Some d when List.contains elem d.elements -> Some d
            | Some d -> Some { d with elements = elem :: d.elements })

    type State = { cwd: string; graph: Graph }

    let interpreter (state: State) cmd : Result<State, string> =
        match cmd with
        | CdRoot -> Ok { state with cwd = "/" }
        | CdUp ->
            Ok
                { state with
                    cwd = state.graph.[state.cwd].parentDirectory }
        | CdInto dir ->
            let path = fullPath state.cwd dir

            let g1 =
                state.graph
                |> ensureDir path state.cwd
                |> addElem state.cwd { name = path; size = 0L }

            Ok { cwd = path; graph = g1 }
        | DirListing dir ->
            let path = fullPath state.cwd dir

            let g1 =
                state.graph
                |> ensureDir path state.cwd
                |> addElem state.cwd { name = path; size = 0L }

            Ok { state with graph = g1 }
        | FileListing(name, size) ->
            let elem = { name = name; size = size }
            let g1 = state.graph |> addElem state.cwd elem
            Ok { state with graph = g1 }
        | NoOp -> Ok state

    // ─────────────────── build graph from input (railway) ───────────────

    let buildGraph (lines: string list) : Result<Graph, string> =
        let initState =
            { cwd = "/"
              graph =
                Map.ofList
                    [ "/",
                      { name = "/"
                        parentDirectory = "/"
                        elements = [] } ] }

        let folder res line =
            res >>= fun st -> parseLine line >>= interpreter st

        lines |> List.fold folder (Ok initState) <!> (fun st -> st.graph)

    // ───────────────────── directory size ─────────────────────────

    let rec dirSize (g: Graph) (path: string) : int64 =
        g.[path].elements
        |> List.sumBy (fun e -> if e.size = 0L then dirSize g e.name else e.size)

    // ─────────────────────── solutions ──────────────────────────────────

    let part1 (lines: string list) : string =
        match buildGraph lines with
        | Error msg -> failwith msg
        | Ok g ->
            g.Keys
            |> Seq.map (dirSize g)
            |> Seq.filter (fun s -> s <= 100_000L)
            |> Seq.sum
            |> string

    let part2 (lines: string list) : string =
        match buildGraph lines with
        | Error msg -> failwith msg
        | Ok g ->
            let totalSpace, needFree = 70_000_000L, 30_000_000L
            let used = dirSize g "/"
            let target = needFree - (totalSpace - used)

            g.Keys
            |> Seq.map (dirSize g)
            |> Seq.filter (fun s -> s >= target)
            |> Seq.min
            |> string

namespace Aoc2022Lib

open System.Text.RegularExpressions

module Day05 =

    type Crate = char
    type Stack = list<Crate>
    type Stacks = list<Stack>

    type Direction = list<int> // [quantity; from; to]
    type Directions = list<Direction>


    let UpdateStackAt index f list =
        if index < 0 || index >= List.length list then
            Error $"Index {index} out of bounds"
        else
            Ok (list |> List.mapi (fun i x -> if i = index then f x else x))

    let TryGet index list =
        if index < 0 || index >= List.length list then
            Error $"Index {index} out of bounds"
        else
            Ok (List.item index list)

    let PopAt index stacks =
        TryGet index stacks
        |> Result.bind (function
            | [] -> Error $"Tried to pop from empty stack {index + 1}"
            | hd :: tl ->
                UpdateStackAt index (fun _ -> tl) stacks
                |> Result.map (fun updated -> (hd, updated)))

    let PopAtPart2 n index stacks =
        TryGet index stacks
        |> Result.bind (fun stack ->
            if List.length stack < n then
                Error $"Not enough crates to move {n} from stack {index + 1}"
            else
                let toMove = List.take n stack
                let remaining = List.skip n stack
                UpdateStackAt index (fun _ -> remaining) stacks
                |> Result.map (fun updated -> (toMove, updated)))

    let PushAt index crate stacks =
        UpdateStackAt index (fun s -> crate :: s) stacks

    let PushAtPart2 index crates stacks =
        UpdateStackAt index (fun s -> crates @ s) stacks

    let GetNumberOfStacks (stackLines: list<string>) : int =
        stackLines
        |> List.head
        |> fun s -> s.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.length

    let GetStackChars (stackLines: list<string>) =
        let numStacks = GetNumberOfStacks stackLines
        let crateLines = stackLines |> List.tail

        let parseLine (line: string) : Crate option list =
            [ 0 .. numStacks - 1 ]
            |> List.map (fun i ->
                let pos = i * 4 + 1

                if pos < line.Length then
                    let ch = line.[pos]
                    if System.Char.IsLetter ch then Some ch else None
                else
                    None)

        crateLines
        |> List.map parseLine
        |> List.transpose
        |> List.map (List.choose id)
        |> List.map List.rev

    let GetDirections (moveLines: string list) =
        moveLines
        |> List.map (fun line ->
            let matches = Regex.Matches(line, @"\d+")
            matches |> Seq.cast<Match> |> Seq.map (fun m -> int m.Value) |> List.ofSeq)

    let ParseInput (lines: list<string>) : Stacks * Directions =
        let stackLines, moveLines =
            lines |> List.takeWhile ((<>) "") |> List.rev, // Stack lines (reversed for bottom-to-top)
            lines |> List.skipWhile ((<>) "") |> List.skip 1 // Skip blank line

        let crates = GetStackChars stackLines
        let directions = GetDirections moveLines
        crates, directions

    let ApplyOneMove part stacks dir : Result<Stacks, string> =
        match dir with
        | [count; fromIdx; toIdx] ->
            let fromIdx = fromIdx - 1
            let toIdx = toIdx - 1
            if part = 1 then
                let rec move i res =
                    match res with
                    | Error _ as e -> e
                    | Ok stacks ->
                        if i = 0 then Ok stacks
                        else
                            PopAt fromIdx stacks
                            |> Result.bind (fun (crate, s1) -> PushAt toIdx crate s1)
                            |> move (i - 1)
                move count (Ok stacks)
            else
                PopAtPart2 count fromIdx stacks
                |> Result.bind (fun (crates, s1) -> PushAtPart2 toIdx crates s1)
        | _ -> Error $"Invalid direction format: {dir}"


    let ApplyAllMoves directions stacks part : Result<Stacks, string> =
        List.fold
            (fun acc dir -> Result.bind (fun stacks -> ApplyOneMove part stacks dir) acc)
            (Ok stacks)
            directions

    let ResultToTopCrates stacks =
        stacks
        |> List.map (function [] -> ' ' | x :: _ -> x)
        |> System.String.Concat

    let part1 input =
        let stacks, directions = ParseInput input
        match ApplyAllMoves directions stacks 1 with
        | Ok finalStacks -> ResultToTopCrates finalStacks
        | Error msg -> $"Error: {msg}"


    let part2 input =
        let stacks, directions = ParseInput input
        match ApplyAllMoves directions stacks 2 with
        | Ok finalStacks -> ResultToTopCrates finalStacks
        | Error msg -> $"Error: {msg}"

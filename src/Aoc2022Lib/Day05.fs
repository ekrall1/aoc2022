namespace Aoc2022Lib

open Aoc2022Lib.Utils
open System.Text.RegularExpressions

module Day05 =

    type Crate = char
    type Stack = list<Crate>
    type Stacks = list<Stack>

    type Direction = list<int> // [quantity; from; to]
    type Directions = list<Direction>


    let UpdateStackAt (idx: int) (f: Stack -> Stack) (stacks: Stacks) : Stacks =
        stacks |> List.mapi (fun i s -> if i = idx then f s else s)

    let PopAt (index: int) (stacks: Stacks) : Crate * Stacks =
        match List.item index stacks with
        | hd :: tl ->
            let updated = stacks |> List.mapi (fun i s -> if i = index then tl else s)
            (hd, updated)
        | [] -> failwithf "Tried to pop from empty stack %d" (index + 1)

    let PushAt (index: int) (crate: Crate) (stacks: Stacks) : Stacks =
        stacks |> List.mapi (fun i s -> if i = index then crate :: s else s)

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
            [0 .. numStacks - 1]
            |> List.map (fun i ->
                let pos = i * 4 + 1
                if pos < line.Length then
                    let ch = line.[pos]
                    if System.Char.IsLetter ch then Some ch else None
                else None
            )

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

    let ApplyOneMove (stacks: Stacks) (dir: Direction) : Stacks =
        let count = dir.[0]
        let fromIdx = dir.[1] - 1
        let toIdx = dir.[2] - 1

        let rec move i stacks =
            if i = 0 then stacks
            else
                let crate, newStacks = PopAt fromIdx stacks
                let updatedStacks = PushAt toIdx crate newStacks
                move (i - 1) updatedStacks

        move count stacks

    let ApplyAllMoves (directions: Directions) (stacks: Stacks) : Stacks =
        directions |> List.fold ApplyOneMove stacks

    let part1 input =
        let stacks, directions = ParseInput input
        let finalStacks = ApplyAllMoves directions stacks

        finalStacks
        |> List.map (function
            | [] -> ' '
            | x :: _ -> x)
        |> System.String.Concat


    let part2 input = "not implemented"

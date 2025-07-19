namespace Aoc2022Lib

open Aoc2022Lib.Railway


module Day09 =

    let getOrientation (dir: char) : Result<int * int, string> =
        match dir with
        | 'R' -> Ok(1, 0)
        | 'L' -> Ok(-1, 0)
        | 'U' -> Ok(0, 1)
        | 'D' -> Ok(0, -1)
        | _ -> Error "invalid direciton"


    let newTail (hd: int * int) (tl: (int * int) list) =

        let follow hd' tl' =
            let xDiff = fst hd' - fst tl'
            let yDiff = snd hd' - snd tl'

            match xDiff, yDiff with
            | 0, 0
            | 1, 0
            | 1, 1
            | 1, -1
            | -1, 0
            | -1, 1
            | -1, -1
            | 0, 1
            | 0, -1 -> tl'
            | 2, 0
            | -2, 0 -> fst tl' + sign xDiff * 1, snd tl'
            | 0, 2
            | 0, -2 -> fst tl', snd tl' + sign yDiff * 1
            | _, _ -> fst tl' + sign xDiff * 1, snd tl' + sign yDiff * 1

        let rec makeNewLst prev rest acc =
            match rest with
            | [] -> List.rev acc
            | hd :: tl ->
                let nxt = follow prev hd
                makeNewLst nxt tl (nxt :: acc)

        makeNewLst hd tl []



    let followAllDirections
        (lines: string list)
        (startP1: int * int)
        (startP2: (int * int) list)
        : Result<Set<int * int>, string> =
        let rec loop
            (lines: string list)
            (p1: int * int)
            (p2: (int * int) list)
            (visited: Set<int * int>)
            : Result<Set<int * int>, string> =
            match lines with
            | [] -> Ok visited
            | hd :: tl ->
                let parts = hd.Split(' ')
                let moveSteps = int parts.[1]

                getOrientation parts.[0].[0]
                >>= fun (dx, dy) ->
                    // Iterate moveSteps times, updating both p1 and p2, and accumulating visited positions
                    let (finalP1, finalP2, visited') =
                        [ 1..moveSteps ]
                        |> List.fold
                            (fun (curP1, curP2, acc) _ ->
                                let x1, y1 = curP1
                                let newP1 = x1 + dx, y1 + dy
                                let newP2 = newTail newP1 curP2
                                newP1, newP2, acc |> Set.add (List.last newP2))
                            (p1, p2, visited)

                    loop tl finalP1 finalP2 visited'

        // Start with only the initial position of p2 visited
        let initialVisited = Set.empty |> Set.add (List.last startP2)
        loop lines startP1 startP2 initialVisited

    let part1 (lines: string list) : string =
        let initialHead = 0, 0
        let initialTail = [ (0, 0) ]

        match followAllDirections lines initialHead initialTail with
        | Ok s -> s.Count.ToString()
        | Error msg -> failwith msg

    let part2 (lines: string list) : string =
        let initialHead = 0, 0
        let initialTail = [ 0, 0; 0, 0; 0, 0; 0, 0; 0, 0; 0, 0; 0, 0; 0, 0; 0, 0 ]

        match followAllDirections lines initialHead initialTail with
        | Ok s -> s.Count.ToString()
        | Error msg -> failwith msg

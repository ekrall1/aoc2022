namespace Aoc2022Lib


module Day08 =

    type Grid = int array array

    let MakeGridRow (str: string) =
        str |> Seq.map (fun ch -> int ch - int '0') |> Array.ofSeq

    let MakeGrid (input: string list) : Grid =
        input |> List.map (fun line -> line |> MakeGridRow) |> List.toArray

    let LookLeftAndRight (grid: Grid) =
        let rows = grid.Length
        let cols = grid.[0].Length

        let left = Array2D.create rows cols -1
        let right = Array2D.create rows cols -1

        for r = 0 to rows - 1 do
            let mutable maxHeight = -1

            for c = 0 to cols - 1 do
                left.[r, c] <- maxHeight
                maxHeight <- max maxHeight grid.[r].[c]

        for r = 0 to rows - 1 do
            let mutable maxHeight = -1

            for c = cols - 1 downto 0 do
                right.[r, c] <- maxHeight
                maxHeight <- max maxHeight grid.[r].[c]

        left, right

    let LookUpAndDown (grid: Grid) =
        let rows = grid.Length
        let cols = grid.[0].Length

        let up = Array2D.create rows cols -1
        let down = Array2D.create rows cols -1

        for c = 0 to cols - 1 do
            let mutable maxHeight = -1

            for r = 0 to rows - 1 do
                up.[r, c] <- maxHeight
                maxHeight <- max maxHeight grid.[r].[c]

        for c = 0 to cols - 1 do
            let mutable maxHeight = -1

            for r = rows - 1 downto 0 do
                down.[r, c] <- maxHeight
                maxHeight <- max maxHeight grid.[r].[c]

        up, down

    let CountVisibilityP1 (grid: Grid) (left: int array2d) (right: int array2d) (up: int array2d) (down: int array2d) =

        let rows = grid.Length
        let cols = grid.[0].Length
        let mutable ctr = 0

        for r = 0 to rows - 1 do
            for c = 0 to cols - 1 do
                let h = grid.[r].[c]

                if h > left.[r, c] || h > right.[r, c] || h > up.[r, c] || h > down.[r, c] then
                    ctr <- ctr + 1

        ctr

    let rec LoopUp (h: int) (grid: Grid) r c acc =
        if r = 0 then
            acc
        else
            let r' = r - 1

            if grid.[r'][c] >= h then
                acc + 1
            else
                LoopUp h grid r' c (acc + 1)

    let rec LoopDown h (grid: Grid) r c acc rows =
        if r = rows - 1 then
            acc
        else
            let r' = r + 1

            if grid.[r'][c] >= h then
                acc + 1
            else
                LoopDown h grid r' c (acc + 1) rows

    let rec LoopLeft h (grid: Grid) r c acc =
        if c = 0 then
            acc
        else
            let c' = c - 1

            if grid.[r][c'] >= h then
                acc + 1
            else
                LoopLeft h grid r c' (acc + 1)

    let rec LoopRight h (grid: Grid) r c acc cols =
        if c = cols - 1 then
            acc
        else
            let c' = c + 1

            if grid.[r][c'] >= h then
                acc + 1
            else
                LoopRight h grid r c' (acc + 1) cols


    let CountVisibilityP2 (grid: Grid) =

        let rows = grid.Length
        let cols = grid.[0].Length
        let mutable best = 0

        for r = 0 to rows - 1 do
            for c = 0 to cols - 1 do
                let h = grid.[r].[c]
                let upctr = LoopUp h grid r c 0
                let downctr = LoopDown h grid r c 0 rows
                let leftctr = LoopLeft h grid r c 0
                let rightctr = LoopRight h grid r c 0 cols
                let score = upctr * downctr * leftctr * rightctr

                if score > best then
                    best <- score

        best

    let part1 (lines: string list) : string =
        let grid = lines |> MakeGrid
        let left, right = LookLeftAndRight grid
        let up, down = LookUpAndDown grid
        CountVisibilityP1 grid left right up down |> string


    let part2 (lines: string list) : string =
        let grid = lines |> MakeGrid
        CountVisibilityP2 grid |> string

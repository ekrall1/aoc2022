/// <summary>
/// Advent of Code 2022 — Day 19
/// </summary>
module Aoc2022Lib.Day19

open System
open System.Text.RegularExpressions
open Google.OrTools.LinearSolver

type ParseError =
    | InvalidLine of string
    | InvalidLinePattern of string

type Cost = { ore: int; clay: int; obsidian: int }

type Blueprint =
    { id: int
      oreRobotCost: Cost
      clayRobotCost: Cost
      obsidianRobotCost: Cost
      geodeRobotCost: Cost }

[<Struct>]
type Robot =
    | Ore
    | Clay
    | Obsidian
    | Geode

[<RequireQualifiedAccess>]
module Robot =
    let all: Robot[] = [| Robot.Ore; Robot.Clay; Robot.Obsidian; Robot.Geode |]

    let toString =
        function
        | Robot.Ore -> "ore"
        | Robot.Clay -> "clay"
        | Robot.Obsidian -> "obsidian"
        | Robot.Geode -> "geode"

    let tryParse (s: string) =
        match s.Trim().ToLowerInvariant() with
        | "ore" -> Some Robot.Ore
        | "clay" -> Some Robot.Clay
        | "obsidian" -> Some Robot.Obsidian
        | "geode" -> Some Robot.Geode
        | _ -> None

    let index =
        function
        | Robot.Ore -> 0
        | Robot.Clay -> 1
        | Robot.Obsidian -> 2
        | Robot.Geode -> 3


[<Literal>]
let BlueprintPattern =
    @"^\s*Blueprint\s+(\d+):\s*Each\s+ore\s+robot\s+costs\s+(\d+)\s+ore\.\s*Each\s+clay\s+robot\s+costs\s+(\d+)\s+ore\.\s*Each\s+obsidian\s+robot\s+costs\s+(\d+)\s+ore\s+and\s+(\d+)\s+clay\.\s*Each\s+geode\s+robot\s+costs\s+(\d+)\s+ore\s+and\s+(\d+)\s+obsidian\.\s*$"

let private normalizeBlueprint (s: string) =
    Regex.Replace(s, @"\s*\r?\n\s*", " ").Trim()

let matchBlueprint (s: string) : Result<Match, ParseError> =
    let s' = normalizeBlueprint s

    let m =
        Regex.Match(s', BlueprintPattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnorePatternWhitespace)

    if m.Success then Ok m else Error(InvalidLine s')

let getIdAndCosts (m: Match) : int * int * int * int * int * int * int =
    let intOf (i: int) = int m.Groups.[i].Value
    let id = intOf 1
    let oreOre = intOf 2
    let clayOre = intOf 3
    let obsOre = intOf 4
    let obsClay = intOf 5
    let geoOre = intOf 6
    let geoObs = intOf 7
    id, oreOre, clayOre, obsOre, obsClay, geoOre, geoObs

let cost (o: int) (c: int) (ob: int) : Cost = { ore = o; clay = c; obsidian = ob }

// ---------------- parsing input ----------------
let parseBlueprintCosts (s: string) : Result<Blueprint, ParseError> =
    match matchBlueprint s with
    | Error _ -> Error(InvalidLinePattern s)
    | Ok m ->
        let id, oreOre, clayOre, obsOre, obsClay, geoOre, geoObs = getIdAndCosts m

        Ok
            { id = id
              oreRobotCost = cost oreOre 0 0
              clayRobotCost = cost clayOre 0 0
              obsidianRobotCost = cost obsOre obsClay 0
              geodeRobotCost = cost geoOre 0 geoObs }

let private groupBlueprints (lines: string list) : string list =
    let finish (acc: string list) (cur: string list) : string list =
        if List.isEmpty cur then
            acc
        else
            (String.Join("\n", List.rev cur)) :: acc

    let rec go (acc: string list) (cur: string list) (rest: string list) : string list =
        match rest with
        | [] -> List.rev (finish acc cur)
        | (h: string) :: t ->
            let startsBlueprint =
                h.TrimStart([| ' '; '\t' |]).StartsWith("Blueprint", StringComparison.OrdinalIgnoreCase)

            if startsBlueprint && not (List.isEmpty cur) then
                go (finish acc cur) [ h ] t
            elif String.IsNullOrWhiteSpace(h) then
                go (finish acc cur) [] t
            else
                go acc (h :: cur) t

    go [] [] lines

let parseInput (lst: string list) : Result<Blueprint list, ParseError> =
    let blocks = groupBlueprints lst

    let rec loop bs acc =
        match bs with
        | [] -> Ok(List.rev acc)
        | b :: bt ->
            match parseBlueprintCosts b with
            | Ok bp -> loop bt (bp :: acc)
            | Error e -> Error e

    loop blocks []

// -------------- setup model --------------------
let getCostByRobotType (robot: Robot) (bp: Blueprint) : Robot -> float =
    // pick the Cost record for this robot
    let c =
        match robot with
        | Robot.Ore -> bp.oreRobotCost
        | Robot.Clay -> bp.clayRobotCost
        | Robot.Obsidian -> bp.obsidianRobotCost
        | Robot.Geode -> bp.geodeRobotCost

    fun mineral ->
        match mineral with
        | Robot.Ore -> float c.ore
        | Robot.Clay -> float c.clay
        | Robot.Obsidian -> float c.obsidian
        | Robot.Geode -> 0.0

let costArray (bp: Blueprint) : Cost[] =
    [| bp.oreRobotCost; bp.clayRobotCost; bp.obsidianRobotCost; bp.geodeRobotCost |]

let coeff (m: Robot) (r: Robot) (blueprint: Blueprint) =
    let f = getCostByRobotType r blueprint
    f m

let getStartupVars (solver: Solver) (times: int[]) : Variable[,] =
    let robots = Robot.all

    Array2D.init times.Length robots.Length (fun ti ri ->
        solver.MakeIntVar(0.0, 1.0, $"start[{times.[ti]},{Robot.toString robots.[ri]}]"))

let getOperateVars (solver: Solver) (times: int[]) : Variable[,] =
    let robots = Robot.all

    Array2D.init times.Length robots.Length (fun ti ri ->
        solver.MakeIntVar(0.0, float times.Length, $"operate[{times.[ti]},{Robot.toString robots.[ri]}]"))

let getInventoryVars (solver: Solver) (times: int[]) : Variable[,] =
    let robots = Robot.all // robots is a proxy for mineral types

    Array2D.init times.Length robots.Length (fun ti ri ->
        solver.MakeIntVar(
            0.0,
            float times.Length * float robots.Length,
            $"inventory[{times.[ti]},{Robot.toString robots.[ri]}]"
        ))

let addStartupOperateConstraints (solver: Solver) (times: int[]) (startUp: Variable[,]) (operate: Variable[,]) : unit =

    let robots = Robot.all
    let T, R = times.Length, robots.Length

    for ri = 0 to R - 1 do
        let offset = if robots.[ri] = Robot.Ore then 1.0 else 0.0

        // t = 0: operate[0,ri] = offset
        let c0 = solver.MakeConstraint(offset, offset)
        c0.SetCoefficient(operate.[0, ri], 1.0)

        // t >= 1: operate[t,ri] - sum_{tau=0..t-1} startUp[tau,ri] = offset
        for ti = 1 to T - 1 do
            let c = solver.MakeConstraint(offset, offset)
            c.SetCoefficient(operate.[ti, ri], 1.0)

            for tau = 0 to ti - 1 do
                c.SetCoefficient(startUp.[tau, ri], -1.0)

let addBuildOnePerMinuteConstraint (solver: Solver) (times: int[]) (startUp: Variable[,]) : unit =

    let robots = Robot.all
    let T, R = times.Length, robots.Length

    for ti = 0 to T - 1 do
        // Σ_r startUp[ti, r] ≤ 1
        let c = solver.MakeConstraint(System.Double.NegativeInfinity, 1.0)

        for ri = 0 to R - 1 do
            c.SetCoefficient(startUp.[ti, ri], 1.0)

let private addInventoryConstraint_t0
    (solver: Solver)
    (startUp: Variable[,])
    (operate: Variable[,])
    (inventory: Variable[,])
    (bp: Blueprint)
    (mi: int)
    : unit =

    let robots = Robot.all
    let eq = solver.MakeConstraint(0.0, 0.0) // build equality: LHS == 0
    eq.SetCoefficient(inventory.[0, mi], 1.0)
    eq.SetCoefficient(operate.[0, mi], -1.0)

    for rj = 0 to robots.Length - 1 do
        let a = coeff robots.[mi] robots.[rj] bp

        if a <> 0.0 then
            eq.SetCoefficient(startUp.[0, rj], a)

let private addInventoryConstraint_t
    (solver: Solver)
    (startUp: Variable[,])
    (operate: Variable[,])
    (inventory: Variable[,])
    (bp: Blueprint)
    (mi: int)
    (ti: int)
    : unit =

    let robots = Robot.all
    let eq = solver.MakeConstraint(0.0, 0.0)
    eq.SetCoefficient(inventory.[ti, mi], 1.0)
    eq.SetCoefficient(inventory.[ti - 1, mi], -1.0)
    eq.SetCoefficient(operate.[ti, mi], -1.0)

    for rj = 0 to robots.Length - 1 do
        let a = coeff robots.[mi] robots.[rj] bp

        if a <> 0.0 then
            eq.SetCoefficient(startUp.[ti, rj], a)

let addInventoryConstraints
    (solver: Solver)
    (times: int[])
    (startUpRobot: Variable[,])
    (operateRobot: Variable[,])
    (inventory: Variable[,])
    (blueprint: Blueprint)
    =

    let robots = Robot.all
    let T, R = times.Length, robots.Length

    for mi = 0 to R - 1 do
        addInventoryConstraint_t0 solver startUpRobot operateRobot inventory blueprint mi

        for ti = 1 to T - 1 do
            addInventoryConstraint_t solver startUpRobot operateRobot inventory blueprint mi ti

let addEnsureInventoryBeforeStartConstraints
    (solver: Solver)
    (times: int[])
    (startUp: Variable[,])
    (inventory: Variable[,])
    (bp: Blueprint)
    =

    let robots = Robot.all
    let T, R = times.Length, robots.Length

    for t = 1 to T - 1 do
        for mi = 0 to R - 1 do
            let c = solver.MakeConstraint(Double.NegativeInfinity, 0.0)
            // Σ_r cost(mineral mi, robot r) * startUp[t, r] - inventory[t-1, mi] ≤ 0
            c.SetCoefficient(inventory.[t - 1, mi], -1.0)

            for ri = 0 to R - 1 do
                let a = (getCostByRobotType robots.[ri] bp) robots.[mi]

                if a <> 0.0 then
                    c.SetCoefficient(startUp.[t, ri], a)

let setObjectiveMaxGeodes (solver: Solver) (times: int[]) (inventory: Variable[,]) : unit =

    let lastT = times.Length - 1
    let geodeIdx = Robot.index Robot.Geode

    let obj = solver.Objective()
    obj.SetCoefficient(inventory.[lastT, geodeIdx], 1.0)
    obj.SetMaximization()

// --------------------- debug -----------------------
let dumpSchedule (times: int[]) (startUp: Variable[,]) (operate: Variable[,]) (inventory: Variable[,]) (bp: Blueprint) =

    let robots = Robot.all
    let name r = Robot.toString r
    let T, R = times.Length, robots.Length
    let v (x: Variable) = x.SolutionValue()

    let printRow (label: string) (cells: string array) =
        printfn "  %-8s %s" label (String.Join("  ", cells))

    printfn "---- PLAN DUMP ----"

    for ti = 0 to T - 1 do
        printfn "t = %d" times.[ti]

        // startups at t
        let startCells =
            [| for ri in 0 .. R - 1 -> sprintf "%s=%d" (name robots.[ri]) (int (v startUp.[ti, ri])) |]

        printRow "start:" startCells

        // operate (cumulative active robots) at t
        let opCells =
            [| for ri in 0 .. R - 1 -> sprintf "%s=%d" (name robots.[ri]) (int (v operate.[ti, ri])) |]

        printRow "operate:" opCells

        // inventory (ore, clay, obsidian, geode) at t
        let invCells =
            [| for mi in 0 .. R - 1 -> sprintf "%s=%d" (name robots.[mi]) (int (v inventory.[ti, mi])) |]

        printRow "inv:" invCells

        // produced this minute (equal to operate[t,m])
        let prodCells =
            [| for mi in 0 .. R - 1 -> sprintf "%s=%d" (name robots.[mi]) (int (v operate.[ti, mi])) |]

        printRow "prod:" prodCells

        // consumed this minute (sum of startup costs at t by mineral)
        let consCells =
            [| for mi in 0 .. R - 1 ->
                   let mutable s = 0.0

                   for ri in 0 .. R - 1 do
                       let a = (getCostByRobotType robots.[ri] bp) robots.[mi]

                       if a <> 0.0 then
                           s <- s + a * v startUp.[ti, ri]

                   sprintf "%s=%d" (name robots.[mi]) (int s) |]

        printRow "cons:" consCells

    printfn "--------------"

// --------------------- solve -----------------------
let solve (blueprint: Blueprint) (timePeriods: int) =
    let solver = Solver.CreateSolver("CBC_MIXED_INTEGER_PROGRAMMING")

    if isNull solver then
        failwith "Could not load CBC"

    let times = [| 0 .. (timePeriods - 1) |]
    let robots = Robot.all

    // robot startup variables (binary)
    let startUpRobot = getStartupVars solver times

    // robot operating variables (integer)
    let operateRobot = getOperateVars solver times

    addStartupOperateConstraints solver times startUpRobot operateRobot

    addBuildOnePerMinuteConstraint solver times startUpRobot

    let mineralInventory = getInventoryVars solver times

    addInventoryConstraints solver times startUpRobot operateRobot mineralInventory blueprint

    addEnsureInventoryBeforeStartConstraints solver times startUpRobot mineralInventory blueprint

    setObjectiveMaxGeodes solver times mineralInventory

    let status = solver.Solve()

    match status with
    | Solver.ResultStatus.OPTIMAL
    | Solver.ResultStatus.FEASIBLE ->
        dumpSchedule times startUpRobot operateRobot mineralInventory blueprint
        solver.Objective().Value() |> int
    | _ ->
        (printfn "Solver failed to solve with status %A" status
         -1)


let part1 (lines: string list) =
    match parseInput lines with
    | Error err ->
        match err with
        | InvalidLine s -> $"Error parsing line: %s{s}"
        | InvalidLinePattern s -> $"Error parsing line (pattern mismatch): %s{s}"
    | Ok blueprints ->
        let results = blueprints |> List.map (fun bp -> bp.id * solve bp 24)
        let total = results |> List.sum
        total |> string

let part2 (lines: string list) =
    match parseInput lines with
    | Error err ->
        match err with
        | InvalidLine s -> $"Error parsing line: %s{s}"
        | InvalidLinePattern s -> $"Error parsing line (pattern mismatch): %s{s}"
    | Ok blueprints ->
        let maxBluePrints = if blueprints.Length < 3 then blueprints.Length else 3
        let results = blueprints.[.. maxBluePrints - 1] |> List.map (fun bp -> solve bp 32)
        let total = results |> List.fold (fun acc res -> acc * res) 1
        total |> string

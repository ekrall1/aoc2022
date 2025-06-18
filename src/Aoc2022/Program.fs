open Argu
open Aoc2022Lib.Dispatch
open Aoc2022Lib.ReadInput


type CLIArguments =
    | [<Mandatory>] Day of int
    | [<Mandatory>] Part of int
    | [<Mandatory>] InputFile of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Specify the day number."
            | Part _ -> "Specify the part number (1 or 2)."
            | InputFile _ -> "Specify the input file path."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "aoc2022")
    let results = parser.Parse(argv)

    let day = results.GetResult <@ Day @>
    let part = results.GetResult <@ Part @>
    let inputFile = results.GetResult <@ InputFile @>

    let input = read inputFile

    match Map.tryFind day runners with
    | Some runner ->
        let result =
            match part with
            | 1 -> runner.part1 input
            | 2 -> runner.part2 input
            | _ -> failwithf "Invalid part: %d" part
        printfn "Result: %s" result
    | None ->
        failwithf "Day %d not implemented" day

    0

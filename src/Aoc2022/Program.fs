open Argu

type CLIArguments =
    | [<Mandatory; AltCommandLine("-d")>] Day of int
    | [<Mandatory; AltCommandLine("-p")>] Part of int
    | [<Mandatory; AltCommandLine("-i")>] Input of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Specify the day number."
            | Part _ -> "Specify the part number (1 or 2)."
            | Input _ -> "Specify the input file path."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "aoc2022")
    let results = parser.Parse(argv)

    let day = results.GetResult <@ Day @>
    let part = results.GetResult <@ Part @>
    let input = results.GetResult <@ Input @>

    let part1_fn, part2_fn =
        match day with
        | 1 -> "not implemented"
        | _ -> failwith "Invalid day"

    let result = 
        match part with
        | 1 -> (part1_fn input)
        | 2 -> (part2_fn input)
        | _ -> failWith "Invalid part"

    printfn "Result: %s" result
    0

namespace Aoc2022Lib

module Utils =

    let parseInt (s: string) : Result<int, string> =
        match System.Int32.TryParse s with
        | true, n -> Ok n
        | false, _ -> Error $"Invalid integer: '{s}'"

    let map2 (f: 'a -> 'b -> 'c) (r1: Result<'a, 'e>) (r2: Result<'b, 'e>) : Result<'c, 'e> =
        match r1, r2 with
        | Ok a, Ok b -> Ok(f a b)
        | Error e, _ -> Error e
        | _, Error e -> Error e

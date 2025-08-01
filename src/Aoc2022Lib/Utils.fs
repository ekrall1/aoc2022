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

    let pop (stack: 'a list) : ('a * 'a list) option =
        match stack with
        | [] -> None
        | x :: xs -> Some(x, xs)

    let push (item: 'a) (stack: 'a list) : 'a list = item :: stack

    let rec gcf (a: int64) (b: int64) : int64 =
        match b with
        | 0L -> a
        | _ -> gcf b (a % b)

    let lcm (a: int64) (b: int64) : int64 = (abs a / gcf (abs a) (abs b)) * abs b

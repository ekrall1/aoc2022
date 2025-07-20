namespace Aoc2022Lib

module Railway =
    let inline (>>=) r f = Result.bind f r
    let inline (<!>) (r: Result<'a,'b>) f = Result.map f r

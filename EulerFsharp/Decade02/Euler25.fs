// http://projecteuler.net/problem=25
namespace Decade02

module Euler25 =
    let main args =

        let fibo = Seq.unfold (function
                                | a, b, pos when a = bigint.Zero && b = bigint.Zero -> Some((bigint.One, pos), (bigint.One, bigint.Zero, pos + 1))
                                | a, b, pos -> Some((a + b, pos), (a + b, a, pos + 1))
                                ) (bigint.Zero, bigint.Zero, 1)


        printfn "%d" (Seq.pick (function x, _ when (bigint.Log10 x) < 999. -> None | _, pos -> Some (pos)) fibo)

        let _ = System.Console.ReadKey true
        0
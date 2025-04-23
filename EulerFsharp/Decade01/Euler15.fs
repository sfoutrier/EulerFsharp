// http://projecteuler.net/problem=15
namespace Decade05

module Euler10 =
    let main args =
        // the result is the number of permutations of 2n elements with twice n repetitions (go right and go down)
        // the formula is C(2n)(n,n) = (2n)!/(n!*n!)

        let rec fact = function
            | n when n = bigint.One -> bigint.One
            | n -> n * fact (n - bigint.One)

        let permutations n = fact ((bigint 2) * n) /(( fact n ) * ( fact n ))

        printfn "%O" (permutations (bigint 20))

        let _ = System.Console.ReadKey true
        0
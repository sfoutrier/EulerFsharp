// http://projecteuler.net/problem=27
namespace Decade02

module Euler27 =

    let main args =

        let rec sieve position elements =
            match position, Array.get elements position with
            | n, _ when n = (Array.length elements) - 1 -> elements
            | _, 0 -> sieve (position + 1) elements
            | _, n ->
                Seq.iter
                    (fun i -> Array.set elements (i - 2) 0)
                    (seq {for i in n..(((Array.length elements) + 1) / n) -> n * i})
                sieve (position + 1) elements

        let limit = 100000
        let primesToLimit =
            sieve 0 [|2..limit|] |> Seq.filter (fun i -> i <> 0) |> Set.ofSeq

        let nbPrimes a b =
            Seq.length (
                    Seq.unfold (
                        fun n ->
                            match n * n + a * n + b with
                            | k when k > limit -> raise (System.Exception())
                            | k ->
                                match Set.contains k primesToLimit with
                                | true -> Some ((), n + 1)
                                | false -> None
                    ) 0
            )

        (*printfn "%d" (nbPrimes 1 41)
        printfn "%d" (nbPrimes -79 1601)*)

        let allQuadras =
            Seq.map
                (
                fun i ->
                    Seq.map
                        (
                        fun j ->
                            nbPrimes i j , i, j
                        ) {-999..999}
                ) {-999..999}
            |> Seq.concat

        let nbPrimes, a, b = Seq.maxBy (function x,_,_ -> x) allQuadras

        printfn "%d (n² + %dn + %d) -> %d" nbPrimes a b (a * b)

        let _ = System.Console.ReadKey true
        0
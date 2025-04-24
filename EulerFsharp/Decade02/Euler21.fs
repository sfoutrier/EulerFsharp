// http://projecteuler.net/problem=21
namespace Decade02

module Euler21 =

    let main args =

        let d n = Seq.sum (seq { for i in 1..n/2 do if (n % i = 0) then yield i })

        let allAmicableToN n = Seq.fold (
                                    fun curAmicable i ->
                                        match Set.contains i curAmicable with
                                        | true -> curAmicable
                                        | false ->
                                            let res1 = d i
                                            match d res1 with
                                            | res2 when res2 = i && res1 <> i ->
                                                Set.add i (Set.add res1 curAmicable)
                                            | _ -> curAmicable
                                ) Set.empty {1..n}


        let allAmicableTo10000 = allAmicableToN 10000
        let result = Seq.sum allAmicableTo10000
        printfn "%d" result

        let _ = System.Console.ReadKey true
        0
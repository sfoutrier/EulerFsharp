// http://projecteuler.net/problem=23
namespace Decade02

module Euler23 =
    let main args =

        let d n = Seq.sum (seq { for i in 1..n/2 do if (n % i = 0) then yield i })
        let abondantTo28123 = [ for i in 1..28123 do if (d i) > i then yield i ]
        let abondantTo28123Set = abondantTo28123 |> Set.ofList

        let isSumOfAbondant n = Seq.takeWhile (fun x -> x <= n/2) abondantTo28123
                                |> Seq.exists (fun x -> Set.contains (n - x) abondantTo28123Set)

        let notSumsOfAbondant = seq { for i in 1..28123 do
                                        if not <| isSumOfAbondant i
                                            then yield i }

        printfn "%d" (Seq.sumBy (int64) notSumsOfAbondant)

        let _ = System.Console.ReadKey true
        0
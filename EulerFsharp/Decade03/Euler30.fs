// http://projecteuler.net/problem=30
namespace Decade03

module Euler30 =
    let main args =

        let sumFifthPowOfDigits n =
            Seq.sumBy (fun x -> x * x * x * x * x)
                ( Seq.unfold (function acc when acc = 0 -> None | acc -> Some(acc % 10, acc / 10) ) n )

        // let run then prey for there is no more match after few seconds ...
        let _ =
            Seq.fold ( fun acc ->
                        function
                        |(Some x) -> printfn "%d" (acc + x); acc + x
                        | _ -> acc
                    ) 0
                (Seq.filter (function None -> false | _ -> true)
                    ( Seq.initInfinite ( function 1 -> None | x when x = sumFifthPowOfDigits x -> Some x | _ -> None ) )
                )

        let _ = System.Console.ReadKey true
        0
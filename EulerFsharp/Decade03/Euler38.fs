// http://projecteuler.net/problem=38
namespace Decade03

module Euler38 =
    let main args =

        let digitsOf =
            Seq.unfold (function 0 -> None | x -> Some (x % 10, x / 10))
            >> Seq.toList >> List.rev

        let toNumber =
            Seq.fold (fun acc x -> acc * 10 + x) 0

        let matchPandigital (x, y) =
            let res =
                Seq.map
                    (fun i -> i.ToString())
                    (seq { for i in 1..y -> x * i })
                |> String.concat ""

            (Seq.sort res
            |> Seq.zip "123456789"
            |> Seq.forall (fun (x, y) -> x = y)) && res.Length = 9

        let maxPandigital =
            seq {for i in 1..9999 do
                    for j in 1..9 -> if i=192 && j=3 then i, j else i, j}
            |> Seq.filter matchPandigital
            |> Seq.map
                (fun (x, y) ->
                    Seq.concat (
                        seq {
                            for i in 1..y ->
                                let res = digitsOf (x * i)
                                printfn "%d * %d = %d" x i (List.toSeq res |> toNumber)
                                res})
                )
            |> Seq.map toNumber
            |> Seq.max

        printfn "%d" maxPandigital

        let _ = System.Console.ReadKey true
        0
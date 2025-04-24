// http://projecteuler.net/problem=32
namespace Decade03

module Euler32 =
    let main args =

        let digits = { 1..9 }

        let rec seqOfDigits curDigits =
            seq {
                for i in digits do
                    if List.forall (fun x -> x <> i) curDigits then
                        match List.length curDigits with
                        | 4 -> yield i :: curDigits
                        | _ -> yield! seqOfDigits (i :: curDigits)
            }

        let numFromSeq =
            Seq.fold (fun acc digit -> acc * 10 + digit) 0

        let seqOfFactors digits =
            Seq.map (
                fun x ->
                    numFromSeq (Seq.take x digits), numFromSeq (Seq.skip x digits)
            ) {3..5}
            |> Seq.filter (fun (x, y) -> x > y)

        let allFactors =
            Seq.map seqOfFactors (seqOfDigits []) |> Seq.concat

        let digitsOf =
            Seq.unfold (function 0 -> None | x -> Some (x % 10, x / 10))

        let filterDigitsOf n digits =
            Seq.fold
                (fun x digit -> Seq.filter (fun i -> i <> digit ) x)
                digits (digitsOf n)

        let matchPandigital (x, y) =
            match digits |> filterDigitsOf x |> filterDigitsOf y |> filterDigitsOf (x * y) |> Seq.length with
            | 0 when x * y < 10000 ->  printfn "%d * %d = %d" x y (x * y); true
            | _ -> false

        let result =
            Seq.filter matchPandigital allFactors
            |> Seq.map (fun (x, y) -> x * y)
            |> Seq.distinct
            |> Seq.sum

        printfn "%d" result

        let _ = System.Console.ReadKey true
        0
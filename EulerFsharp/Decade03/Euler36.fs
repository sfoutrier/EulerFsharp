// http://projecteuler.net/problem=36
namespace Decade03

module Euler36 =
    let main args =

        let digitsInBase n =
            Seq.unfold (fun acc -> match acc with 0 -> None | _ -> Some (acc % n, acc / n))

        let isPalindromeInBase n x =
            let listOfDigits =
                digitsInBase n x |> Seq.toList
            Seq.forall
                (fun (x, y) -> x = y)
                (Seq.zip
                    listOfDigits
                    (List.rev listOfDigits))

        let palindromesTo1000000 =
            Seq.filter
                (fun x -> isPalindromeInBase 2 x && isPalindromeInBase 10 x )
                {1..1000000}

        (*Seq.iter
            (fun x ->
                Seq.iter (printf "%d") (digitsInBase 10 x)
                printf " = "
                Seq.iter (printf "%d") (digitsInBase 2 x)
                printfn " (2)" )
            palindromesTo1000000*)

        printfn "%d" (Seq.sum palindromesTo1000000)

        let _ = System.Console.ReadKey true
        0
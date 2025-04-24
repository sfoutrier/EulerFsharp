// http://projecteuler.net/problem=24
namespace Decade02

module Euler24 =
    let main args =

        let rec fact = function
            | n when n < 1 -> raise (System.Exception())
            | 1 -> 1
            | n -> n * fact (n - 1)

        let digits = [0..9]

        let nthPermutation n = Seq.unfold
                                (
                                    function
                                    | [], _ -> None
                                    | [x], _ -> Some(x, ([], 0))
                                    | remainingDigits, remainingPermutations ->
                                        let possibleSubPermutation = fact ((List.length remainingDigits) - 1)
                                        let subPermutations, digitToPick = remainingPermutations % possibleSubPermutation, remainingPermutations / possibleSubPermutation
                                        let curDigit = List.item digitToPick remainingDigits
                                        Some(curDigit, (List.filter (fun x -> x <> curDigit) remainingDigits, subPermutations))

                                ) (digits, n)
        Seq.iter (printf "%d" ) (nthPermutation 999999)

        let _ = System.Console.ReadKey true
        0
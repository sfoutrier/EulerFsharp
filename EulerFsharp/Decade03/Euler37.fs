// http://projecteuler.net/problem=36
namespace Decade03

module Euler37 =
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

        // hope it's enought, else grow the size of it
        let primes =
            sieve 0 [|2..1000000|] |> Seq.filter (fun i -> i <> 0) |> Set.ofSeq

        let toDigits =
            Seq.unfold (fun acc -> match acc with 0 -> None | _ -> Some (acc % 10, acc / 10))
        let fromDigits =
            Seq.toList >> List.reduceBack (fun cur acc -> acc * 10 + cur)

        let isTruncable truncator list =
            Seq.forall
                (fun x ->
                    Set.contains
                        (truncator x list
                        |> fromDigits)
                        primes)
                {1..((List.length list) - 1)}

        let isTruncablePrime x =
            let digits = toDigits x |> Seq.toList
            (isTruncable Seq.skip digits)
                && (isTruncable Seq.take digits)

        let truncables =
            Seq.filter (fun x -> x >= 10) primes
            |> Seq.filter isTruncablePrime
            |> Seq.take 11

        printfn "%d" (Seq.sum truncables)

        //printfn "%b" (isTruncablePrime 3797)

        let _ = System.Console.ReadKey true
        0
// https://projecteuler.net/problem=60
open System
open System.Collections.Generic
open System.Linq

let primesTo maxOfSieve = 

    let rec sieve position elements =
        match Array.length elements with
        | p when p = position -> elements
        | _ ->
            match Array.get elements position with
            | 1 -> ()
            | n -> Seq.iter
                    (fun i -> Array.set elements (i - 1) 1) 
                    (seq {for i in n..((Array.length elements) / n) -> n * i})
            sieve (position + 1) elements

    sieve 0 [|1..maxOfSieve|] |> Array.filter (fun i -> i <> 1)

[<EntryPoint>]
let main argv =

    let nbToFind = 5
    let maxPrime = 10000
    printfn "Computing primes"
    let seedPrimes = primesTo maxPrime
    printfn "Seed primes computed"

    let maxPrimeTesting = (fun i -> i * i) (int64 maxPrime)
    let isPrime i =
        match i with
        | i when int64 i > maxPrimeTesting -> failwith "Going above maxPrime"
        | i ->
            Seq.takeWhile (fun p -> i > p) seedPrimes
            |> Seq.forall (fun p -> i % p <> 0)

    let checkPair (p1, p2) =
        let checkHalfPair (p1, p2) =
            sprintf "%i%i" p1 p2 |> int |> isPrime
        checkHalfPair (p1, p2) && checkHalfPair (p2, p1)

    let findPrimePairsOf prime =
        seedPrimes
        |> Seq.takeWhile ((>) prime)
        |> Seq.filter (fun p -> checkPair (p, prime))
        |> Set.ofSeq

    let rec haveNPairs n prime pairs computedPairs =
        match n with
        | 0 -> Seq.singleton [prime]
        | _ -> Seq.map
                ( fun p ->
                    match Set.intersect pairs (Map.find p computedPairs) with
                    | pairsOfP when Set.count pairsOfP < (n-1) ->
                        //printfn "Not enough pairs for %i (expected %i): %A" p n pairsOfP
                        Seq.empty
                    | pairsOfP ->
                        haveNPairs (n-1) p pairsOfP computedPairs
                ) pairs |> Seq.concat |> Seq.map (fun r -> prime::r) 

    let rec scanPrimes computedPairs acc =
        function
        | [] -> Seq.concat acc
        | p::tail ->
            let pairsOfN = findPrimePairsOf p
            //printfn "%d -> %A" p pairsOfN
            scanPrimes
                (Map.add p pairsOfN computedPairs)
                ((haveNPairs (nbToFind-1) p pairsOfN computedPairs)::acc)
                tail

    let foundPairs = scanPrimes (Map.empty) [] (List.ofArray seedPrimes)
    let pairsWithSum = foundPairs |> Seq.map (fun p -> (List.sum p, p)) |> Seq.sortBy fst |> Seq.toList
    //pairsWithSum |> Seq.iter (printfn "Found %A")

    match pairsWithSum with
    | (first, _)::_ when first < Array.head seedPrimes ->
        printfn "Could have a smaller tuple, try with bigger  maxPrimes"
    | res::_ -> printfn "Definitive result: %A" res
    | [] -> printfn "No result found, increase maxPrimes"

    let _ = Console.ReadLine()
    0 // return an integer exit code
 
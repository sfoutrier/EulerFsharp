// https://projecteuler.net/problem=60
open System
open System.Collections.Generic
open System.Linq

let maxPrime = 1000

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

let seedPrimes = primesTo maxPrime
let maxPrimeTesting = (fun i -> i * i) (int64 maxPrime)
let isPrime i =
    match i with
    | i when int64 i > maxPrimeTesting -> failwith "Going above maxPrime"
    | i ->
        Seq.takeWhile (fun p -> i > p) seedPrimes
        |> Seq.forall (fun p -> i % p <> 0)

[<EntryPoint>]
let main argv =
    let nbToFind = 4
    let indexedPrimes = Seq.indexed seedPrimes |> Map.ofSeq

    let mapPrimes p = Map.find p indexedPrimes
    let sumCandidates = Array.sumBy mapPrimes
    let mapCandidate = Array.map mapPrimes
    let initialCandidates = [|0..nbToFind-1|]

    printfn "Initial candidates: %A" (mapCandidate initialCandidates)

    // splits a candidate to n others + their sum
    let split = function
        | c, _ ->
            seq {
                for i in 0..Array.length c - 1 ->
                    match c.[i] with
                    // if last element or lower than the next one (to keep only ordered tuples)
                    | j when i = Array.length c - 1 || j + 1 < c.[i+1] ->
                        let newCandidate = Array.copy c
                        newCandidate.[i] <- j + 1
                        Some(newCandidate)
                    | _ -> None }
            |> Seq.filter (function None -> false | _ -> true)
            |> Seq.map (function Some(c) -> c, sumCandidates c)
            |> Seq.toList

    let tree = new SortedDictionary<int, int[] Set>()

    let dump () =
        for segment in tree do
            printf "%i -> " segment.Key
            for candidate in segment.Value do
                printf "%A " (mapCandidate candidate)
            printfn ""

    let insert (candidate, sum) =
        match tree.ContainsKey sum with
        | false -> tree.Add (sum, Set.singleton candidate)
        | true -> tree.[sum] <- Set.add candidate tree.[sum]

    let pop () =
        let key = tree.Keys.First()
        let segment = tree.[key]
        let first = Set.minElement segment
        match Set.count segment with
        | 1 -> tree.Remove(key) |> ignore
        | _ -> tree.[key] <- Set.remove first segment
        first

    let checkPair (x, y) =
        let toCheck = sprintf "%i%i" (mapPrimes x) (mapPrimes y) |> int
        //printfn "Testing prime of %i" toCheck
        toCheck |> isPrime

    let checkCandidate (c:int[]) =
        seq { for i in 0..nbToFind-1 do
                for j in 0..nbToFind-1 do
                    yield (i, j) }
        |> Seq.forall (fun (x, y) -> x = y || checkPair (c.[x], c.[y]))

    let rec findMatchingCandidate () =
        //dump ()
        let candidate = pop()
        //printfn "Checking %A" (mapCandidate candidate)
        match checkCandidate candidate with
        | true -> candidate
        | false ->
            split (candidate, sumCandidates candidate) |> List.iter insert
            findMatchingCandidate ()

    insert (initialCandidates, sumCandidates initialCandidates)
    let matchingCandidate = findMatchingCandidate ()

    printfn "%A" (mapCandidate matchingCandidate)

    let _ = Console.ReadLine()
    0 // return an integer exit code
 
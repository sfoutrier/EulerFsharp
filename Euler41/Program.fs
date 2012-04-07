// http://projecteuler.net/problem=41

[<EntryPoint>]
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

    // all primes to sqrt of the max possible pandigital number
    let maxOfSieve = int(sqrt 987654321.)
    let primes =
        sieve 0 [|2..maxOfSieve|] |> Seq.filter (fun i -> i <> 0) |> Set.ofSeq

    let isPrime n =
        if n <= maxOfSieve then
            primes.Contains n
        else
            Seq.forall (fun x -> n % x <> 0) primes
   
    let rec allPandigitals n curNumber =
        seq {
            match List.length curNumber with
            | x when x = n -> yield List.rev curNumber
            | _ ->
                for i in 1..n do
                    if List.forall ((<>) i) curNumber then
                        yield! allPandigitals n (i :: curNumber)
        }
        
    let toNumber =
        Seq.fold (fun acc x -> acc * 10 + x) 0

    let allPandigitalsPrimes =
        Seq.map (fun i -> allPandigitals i []) {2..9}
        |> Seq.concat
        |> Seq.map toNumber
        |> Seq.filter isPrime

    //Seq.iter (printfn "%d") allPandigitalsPrimes
    printfn "%d" (Seq.max allPandigitalsPrimes)

    let _ = System.Console.ReadKey true
    0
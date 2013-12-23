let primesTo maxOfSieve =
    let rec sieve position elements =
        if position = maxOfSieve then
            elements
        else
            match Array.get elements position with
            |1 -> sieve (position + 1) elements
            |n -> 
                Seq.iter 
                    (fun i -> Array.set elements (i - 1) 1) 
                    (seq {for i in n..((maxOfSieve) / n) -> n * i})
                sieve (position + 1) elements

    sieve 0 [|1..maxOfSieve|] 
    |> Seq.filter (fun i -> i <> 1)

[<EntryPoint>]
let main args =
    let MIN = 1000
    let MAX = 10000
    let primes = 
        primesTo MAX 
        |> Seq.filter (fun i -> i >= MIN)
        |> Seq.toList
    let primesSet = Set.ofSeq primes

    let rec allDigits x digits =
        match x with
        | 0 -> digits
        | _ -> allDigits (x / 10) (x % 10 :: digits)

    let arePermutation x y =
        List.zip
            (allDigits x [] |> List.sort)
            (allDigits y [] |> List.sort)
        |> List.forall (fun (a, b) -> a = b)

    let filterSeq x remainingPrimes =
        Seq.skipWhile (fun y -> y <= x) remainingPrimes
        // find all sequence begin with 2 primes
        |> Seq.map (fun y -> (x, y, (2 * y - x))) 
        // filter those with 3 primes
        |> Seq.filter (function a, b, c when Set.contains c primesSet -> true | _ -> false)
        // filter on permutations
        |> Seq.filter (fun (a, b, c) -> (arePermutation a b) && (arePermutation a c))
        
    let rec sequences primesList found =
        match primesList with
        | [] -> found
        | head::remainingPrimes ->
            let nextResults = filterSeq head remainingPrimes
            sequences remainingPrimes (Seq.append found nextResults)

    sequences primes Seq.empty
    |> Seq.iter (fun (a, b, c) -> printfn "%d %d %d" a b c)

    let _ = System.Console.ReadKey true
    0
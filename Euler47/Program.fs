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
    let MAX = 1000 * 1000 * 1000
    let MAGIC_NUMBER = 4

    let root2 x = int ((double x) ** 0.5)
    let primes = primesTo (root2 MAX) |> Seq.toList

    let rec divisors number foundDivisors primes =
        match number with
        | 1 -> (number, foundDivisors)
        | _ -> 
            match primes with
            | [] -> (number, foundDivisors)
            | prime::remainingPrimes ->
                match number % prime with 
                | 0 -> divisors (number / prime) (prime::foundDivisors) primes
                | _ -> divisors number foundDivisors remainingPrimes
    
    let primesOf x =
        divisors x [] primes |> snd |> Seq.toList

    let count x =
        Seq.fold 
            (fun (cnt, prev) cur -> 
                match cur with 
                | x when x = prev -> (cnt, cur) 
                | _ -> (cnt + 1, cur) )
            (0, 0) x
        |> fst

    let candidates = seq {1..MAX} |> Seq.windowed MAGIC_NUMBER
    let condition candidate =
        let primesOfCandidates = Seq.map primesOf candidate
        Seq.forall (fun l -> count l = MAGIC_NUMBER) primesOfCandidates

    let result = Seq.find condition candidates
    Seq.iter
        (printfn "%A")
        (Seq.map (fun x -> (x, snd (divisors x [] primes))) result)

    let _ = System.Console.ReadKey true
    0
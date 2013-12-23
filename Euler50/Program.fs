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
    let MAX = 1000 * 1000
    let primes = primesTo MAX |> Seq.toList
    let primesSet = Set.ofList primes

    let candidates remainingPrimes =
        Seq.scan (fun (count, sum) prime -> (count + 1, sum + prime)) (0, 0) remainingPrimes
        |> Seq.takeWhile (fun (c, s) -> s < MAX)
        |> Seq.toList
        |> List.rev 
        |> List.tryFind (fun (c, s) -> Set.contains s primesSet)

    let rec allSequences remainingPrimes =
        seq {
            match remainingPrimes with
            | [] -> ()
            | head::tail ->
                match candidates remainingPrimes with 
                | Some(count, sum) -> yield (head, count, sum)
                | None -> ()
                yield! allSequences tail}

    let result = 
        Seq.maxBy 
            (fun (start, count, sum) -> count) 
            (allSequences primes)

    match result with (f, s, t) ->
        printfn "first:%d count:%d sum:%d " f s t

    let _ = System.Console.ReadKey true
    0
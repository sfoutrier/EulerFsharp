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
    let MAX = 100000000
    let SQRT_MAX = 10000

    let primes = primesTo SQRT_MAX |> Set.ofSeq
    let twiceSquaresTo x = 
        Seq.initInfinite (fun i -> 2 * i * i)
        |> Seq.skip 1
        |> Seq.takeWhile (fun square ->
                            x > square)

    let isPrime x = Set.contains x primes

    let condition candidate =
        Seq.exists
            (fun square -> isPrime (candidate - square))
            (twiceSquaresTo candidate)

    let candidates =
        seq { for candidate in 2..MAX do
                if candidate % 2 = 1 && not (isPrime candidate) then 
                    yield candidate}

    let result = Seq.find (fun x -> not (condition x)) candidates
    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
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
    let MAX = 1000000
    let primes = primesTo MAX |> Seq.toList
    let isPrime (n:int64) = 
        if int64(MAX)*int64(MAX) < n then failwith "Not enought primes computed"
        Seq.takeWhile (fun i -> (int64(i)*int64(i))<=n) primes |> Seq.forall (fun i -> n%int64(i) <> 0L)
    let inc n = n+1L

    Seq.unfold (fun (pos,np,e) ->
                    let l = 2L*(inc (e/4L))
                    match isPrime (pos+l) with
                    | true -> Some((inc e,inc np,l),(pos+l,inc np,inc e))
                    | false -> Some((inc e,np,l),(pos+l,np,inc e)))
                (1L, 0L, 0L)
    // find only full square (%4) inc e to count 1 (the middle of the square)
    |> Seq.find (fun (e,np, _) -> e%4L=0L && np*100L/(inc e) < 10L)
    // l is the step to go from one corner to the next one, the edge length is inc l
    |> function (p,np,l) -> printfn "%d" (inc l)

    let _ = System.Console.ReadKey true
    0
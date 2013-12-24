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
    let NB_MATCHES = 7
    let MAX_PRIMES = 1000000
    let MAX_DIGITS = 6

    let rec nextDigits number digits =
        match number with
        | 0 -> digits
        | _ -> nextDigits (number / 10) (number % 10 :: digits)
    let digitsFrom number = nextDigits number []
    let numberFrom digits =
        Seq.fold (fun number digit -> number * 10 + digit) 0 digits

    let rec insert v i l =
        match i, l with
        | 0, xs -> v::xs
        | i, x::xs -> x::insert v (i - 1) xs
        | i, [] -> failwith "index out of range"

    let candidatesWith stableDigits positions =
        seq {for i in 0..9 -> 
                Seq.fold 
                    (fun toInsert position -> insert i position toInsert)
                    stableDigits
                    positions
                |> function 0::tail -> 0 | x -> numberFrom x }

    let rec allPositions min max nb positions =
        seq {
            match nb with
            | 0 -> yield positions
            | _ -> for i in min..(max - nb + 1) do
                    yield! allPositions i max (nb - 1) (i::positions) }

    let rec allNumbers n digits =
        seq {
            match n with
            | 0 -> yield digits
            | 1 -> for i in 1..9 do yield! allNumbers (n - 1) (i::digits)
            | _ -> for i in 0..9 do yield! allNumbers (n - 1) (i::digits)
        }
        
    let primes = primesTo MAX_PRIMES |> Set.ofSeq

    let filterPrimes candidates =
        Seq.filter (function x when Set.contains x primes -> true | _ -> false) candidates

    let candidatesWithLength stableLength variableLength =
        Seq.map
            (fun positions ->
                Seq.map (fun stableDigits ->
                    candidatesWith stableDigits positions
                        |> filterPrimes 
                        |> Seq.toList)
                    (allNumbers stableLength []) 
            )
            (allPositions 0 stableLength variableLength [])
        |> Seq.collect (fun x -> x)

    for i in 2..MAX_DIGITS do
        for j in 1..i-1 do
            candidatesWithLength j (i-j)
                |> Seq.filter (fun x -> List.length x >= NB_MATCHES)
                |> Seq.iter (printfn "%A")

    let _ = System.Console.ReadKey true
    0
// http://projecteuler.net/problem=35

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

    let primesToMillion =
        sieve 0 [|2..1000000|] |> Seq.filter (fun i -> i <> 0) |> Set.ofSeq
        
    let numberToSeq =
        Seq.unfold (fun acc -> match acc with 0 -> None | x -> Some (x % 10, x / 10))
    let seqToNumber =
        Seq.fold (fun acc x -> acc * 10 + x) 0

    let circular n =
        seq {
            let digits = numberToSeq n |> Seq.toArray |> Array.rev
            for i in 1..(Array.length digits) -> 
                Seq.append (Seq.skip i digits) (Seq.take i digits) |> seqToNumber
        }

    let circularPrimes =
        Seq.filter 
            (fun x -> 
                Seq.forall 
                    (fun y -> Set.contains y primesToMillion) 
                    (circular x)
            )
            primesToMillion

    //Seq.iter (printfn "%d") circularPrimes
    printfn "%d" (Seq.length circularPrimes)

    let _ = System.Console.ReadKey true
    0
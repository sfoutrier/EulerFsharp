// http://projecteuler.net/problem=43

[<EntryPoint>]
let main args =

    let primes = [2L;3L;5L;7L;11L;13L;17L]
        

    let toNumber =
        Seq.fold (fun acc x -> acc * 10L + x) 0L

    let rec pandigitals number remainingDigits =
        seq {
            match Set.count remainingDigits with
            | 0 -> yield number |> List.ofSeq |> List.rev
            | _ ->
                for i in remainingDigits do
                    let value = (function x::y::_::_ -> toNumber (List.toSeq [y;x;i]) | _ -> 0L) number
                    if (value = 0L || value % (List.nth primes ((List.length number) - 3)) = 0L) then
                        yield! pandigitals (i::number) (Set.remove i remainingDigits)
        }

    let allPandigitals = pandigitals [] (Set.ofSeq {0L..9L})

    let result = 
        allPandigitals
        |> Seq.sumBy toNumber

    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
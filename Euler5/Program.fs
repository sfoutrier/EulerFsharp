
[<EntryPoint>]
let main args =

    let rec extractRec = function
        | x, y, count when x % y = 0 -> extractRec (x / y, y, count + 1)
        | a -> a
    let extract = function 
        | x, y when y = 1 -> (x, y, 1)
        | x, y when y < 2 || y > x -> (x, y, 0)
        | x, y -> extractRec (x, y, 0)

    let unfolder = function
        | x, _, count when x = 1 && count = 0 -> None
        | x, y, count -> Some(count, (x, y+1))

    let decomposition x =
        Seq.unfold (extract >> unfolder ) (x, 1) |> Seq.toArray

    //Seq.iter ( fun a -> Array.iteri (fun x y -> printf "%d->%d " (x + 1) y ) a; printfn "") (seq {for x in 1..maxValue -> decomposition x})

    let rec pow x y = match y with
                        | 0 -> 1
                        | _ -> x * pow x (y - 1)

    let rec maxArray a b = 
        let lenA, lenB = Array.length a - 1, Array.length b - 1
        if lenA < lenB then maxArray b a else
            [| for i in 0..lenA -> if i <= lenB then 
                                        max (Array.get a i) (Array.get b i)
                                    else Array.get a i |]

    // take the max all primes factors
    let result = Seq.map decomposition {1..20}
                    |> Seq.reduce maxArray
                    //|> Array.iteri (fun index curPow -> printf "%d->%d " (index + 1) curPow)
                    |> Array.fold (fun (i, curProduct) curFactor -> (i + 1, curProduct * (pow i curFactor))) (1, 1)
                    |> snd
    printfn "%d" result

    // the brute force algo that can't work for 20 ...
    (*let infinite = Seq.initInfinite (fun x -> 20 * (x + 1))
    let isDivisableByUpTo x y = Seq.forall (fun z -> y % z = 0) { 1..x }
    let result = Seq.find (isDivisableByUpTo 20) infinite
    printfn "%d" result*)

    let _ = System.Console.ReadKey true
    0
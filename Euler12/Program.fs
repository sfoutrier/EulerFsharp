
[<EntryPoint>]
let main args =

    (*let triangles = Seq.unfold (fun (i, curTriangle) ->
                                    let nextTriangle = curTriangle + i
                                    Some (nextTriangle, (i, nextTriangle)) ) (1, 0)*)

    let divisors x = seq { for y in 1..x do if x % y = 0 then yield y }
    let numbersWithDivisors = Seq.unfold (fun acc -> Some((acc, Seq.length (divisors acc)), acc + 1)) 1
    let consecutivesNumbers = Seq.windowed 2 numbersWithDivisors
    let consecutiveWithAtLeastNDivisors howMuch = Seq.filter (fun x -> Array.sumBy (fun y -> snd y) x >= howMuch) consecutivesNumbers
                                                    |> Seq.map (fun x -> (Array.fold (fun acc y -> acc * fst y) 1 x) / 2 )

    Seq.iter (fun x -> printfn "%d : %d" x (Seq.length (divisors x))) (consecutiveWithAtLeastNDivisors 40)

    let _ = System.Console.ReadKey true
    0
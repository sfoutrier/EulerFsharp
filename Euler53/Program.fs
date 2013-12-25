(*
** Assuming nCr can be written
** (1*2*..*r*(r+1)*..*n)/(1*2*..*r*1*2*..*(n-r))
** = ((r+1)*(r+2)*..*n)/(1*2*..*(n-r))
** = (r+1)/1 * (r+2)/2 * .. * n/(n-r)
** = P(i in 1..(n-r)) (r+i)/i
** the serie is growing on n so  if it exceeds 1M for any value of i, the full product exceeds 1M
*)

[<EntryPoint>]
let main args =
    let THRESHOLD = 1000 * 1000
    let MAX = 100

    let serie n r =
        Seq.unfold 
            (function 
                | p, i when i > n - r -> (*printfn "%d %d %d %d"  n r i p;*) None 
                | p, i ->
                    let p1 = p * (r + i) / i
                    Some(p1, (p1, i + 1)))
            (1, 1)

    //printfn "%A" (serie 23 10 |> Seq.toList)

    let serieMatch n r =
        Seq.tryFind 
            (fun x -> x > THRESHOLD)
            (serie n r)
        |> function None -> false | _ -> true

    let result =
        Seq.fold
            (fun sum state -> match state with true -> sum + 1 | false -> sum)
            0
            (seq {for n in 1..MAX do for r in 1..n -> serieMatch n r })

    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
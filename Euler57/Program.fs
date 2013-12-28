(*
  s'(0) = 2
  s'(i) = x
  s'(i+1) = f(x) = 2 + 1/x
  let x = n/d => f(x) = 2 + d/n = (2n+d)/n

  s(i) = s'(i)-1 =  = n/d - 1 = (n-d)/d
*)

[<EntryPoint>]
let main args =
    let MAX = 1000

    let len n = n.ToString().Length

    Seq.scan (fun (n,d) _ ->
                let n1=n * bigint(2) + d
                let d1=n
                let x = bigint.GreatestCommonDivisor(n1, d1)
                n1/x,d1/x) (bigint 2, bigint 1) {0..MAX}
    |> Seq.map (fun (n,d) ->
                let n1=n-d
                let x=bigint.GreatestCommonDivisor(n1, d)
                n1/x, d/x)
    |> Seq.filter (fun (n,d) -> (len n)>(len d))
    |> Seq.length |> printfn "%d"

    let _ = System.Console.ReadKey true
    0
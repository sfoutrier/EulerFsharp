[<EntryPoint>]
let main args =
    let MAX = 100

    seq {for i in 1..MAX do
            for j in 1..MAX -> bigint.Pow(new bigint(i), j)}
    |> Seq.map (fun v -> v.ToString() |> Seq.fold (fun s c -> s + int(c) - int('0')) 0)
    |> Seq.max
    |> printfn "%A"

    let _ = System.Console.ReadKey true
    0
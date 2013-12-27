[<EntryPoint>]
let main args =
    let MAX = 10000
    let ITER = 50

    let isPalindrome value =
        let valueChars = value.ToString() |> Seq.toArray
        Seq.zip valueChars (Array.rev valueChars)
            |> Seq.forall (fun (a, b) -> a = b)

    let rev value =
        value.ToString() |> Seq.toArray |> Array.rev |> (fun v->new string(v)) |> bigint.Parse

    let step v = v + (rev v)

    let isLychrel (n:int) =
        Seq.scan
            (fun v _ -> step v)
            (step (bigint n))
            {2..ITER}
        |> Seq.tryFind isPalindrome
        |> Option.isNone

    let result = Seq.fold
                    (fun c i -> match isLychrel i with true -> c+1 | false -> c)
                    0
                    {1..MAX}
        
    printfn "%A" result

    let _ = System.Console.ReadKey true
    0
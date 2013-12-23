[<EntryPoint>]
let main args =
    let MAX = 1000L
    let modulo = 10000000000L

    let lastDigitPow x =
        Seq.fold
            (fun acc _ -> (x * acc) % modulo)
            1L
            {1L..x}

    let result =
        Seq.fold
            (fun acc i -> (acc + (lastDigitPow i)) % modulo)
            0L
            {1L..MAX}

    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
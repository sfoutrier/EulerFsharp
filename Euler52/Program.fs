[<EntryPoint>]
let main args =
    let MULTIPLES = 6

    let rec nextDigits number digits =
        match number with
        | 0 -> digits
        | _ -> nextDigits (number / 10) (number % 10 :: digits)
    let digitsFrom number = nextDigits number []

    let multiples x =
        seq {for i in 2..MULTIPLES -> i * x}

    let check x =
        let sortedDigitsOfX = digitsFrom x |> List.sort
        Seq.forall
            (fun y ->
                let sortedDigitsOfY = digitsFrom y |> List.sort
                List.length sortedDigitsOfY = List.length sortedDigitsOfX
                    && Seq.forall2 (=) sortedDigitsOfX sortedDigitsOfY)
            (multiples x)

    let candidatesWithDigits n =
        let min = int(10. ** (float(n-1)))
        let max = int(10. ** (float n))
        seq {min..(max/MULTIPLES)}

    let allCandidates =
        seq {for i in 2..10 do yield! candidatesWithDigits i}

    let result = 
        Seq.find check allCandidates
    printfn "%d %A" result (multiples result)

    let _ = System.Console.ReadKey true
    0
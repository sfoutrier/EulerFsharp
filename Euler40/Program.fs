// http://projecteuler.net/problem=40

[<EntryPoint>]
let main args =

    let digitsOf =
        Seq.unfold (function 0 -> None | x -> Some (x % 10, x / 10))
        >> Seq.toList >> List.rev
            
    let setOfPosToKeep =
        Set.ofList [1;10;100;1000;10000;100000;1000000]

    let seqOfDigits =
        Seq.initInfinite digitsOf
        |> Seq.concat
        |> Seq.mapi (fun i x -> match setOfPosToKeep.Contains (i+1) with false -> None | true -> Some(x))
        |> Seq.choose id
        |> Seq.take setOfPosToKeep.Count

    printfn "%d" (Seq.reduce (*) seqOfDigits)
        
    let _ = System.Console.ReadKey true
    0
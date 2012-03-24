open System.Numerics

[<EntryPoint>]
let main args =
    let value = BigInteger.Pow (bigint 2, 1000)

    let sumDigits i = Seq.reduce (+) (Seq.map (fun curChar -> int curChar - int '0') (i.ToString()))
    
    printfn "%d" (sumDigits value)

    let _ = System.Console.ReadKey true
    0
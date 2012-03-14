
[<EntryPoint>]
let main args =

    let range = seq { for n in 1..999 do if  n % 3 = 0 || n % 5 = 0 then yield n}
    System.Console.WriteLine (Seq.sum range)

    let toto = System.Console.ReadLine()
    System.Console.WriteLine toto
    0
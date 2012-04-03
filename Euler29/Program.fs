// http://projecteuler.net/problem=29

[<EntryPoint>]
let main args =
    
    let res = 
        Seq.fold
            ( fun powSet (i, j) ->
                Set.add (bigint.Pow (bigint(i:int), j)) powSet )
            Set.empty
            ( seq { for i in 2..100 do
                        for j in 2..100 -> (i, j) } )
        |> Set.count

    printfn "%d" res

    let _ = System.Console.ReadKey true
    0
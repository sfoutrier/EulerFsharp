// http://projecteuler.net/problem=28

[<EntryPoint>]
let main args =

    let lastPosition, sum = 
        Seq.fold 
            (
            fun (position, total) i ->
                    Seq.fold
                        (
                        fun (subPos, subTotal) j ->
                            (subPos + 2 * i, subTotal + subPos + 2 * i)
                        ) (position, total) {1..4}
            ) (1, 1) {1..500}

    printfn "%d" sum

    let _ = System.Console.ReadKey true
    0

[<EntryPoint>]
let main args =

    let triangles = Seq.initInfinite (fun x -> x * (x + 1) / 2)

    let countDivisors x =
        Seq.sumBy (function y when x % y = 0 -> 2 | _ -> 0) {1..(int(sqrt(float x)))}

    let result = Seq.find (fun x -> countDivisors x > 500) triangles
    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
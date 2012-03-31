
[<EntryPoint>]
let main args =
    
    let facto100 = Seq.reduce (*) (seq {for i in 1..100 -> bigint i})
    let result = Seq.sumBy (fun x -> ((int)x) - ((int)'0')) (facto100.ToString())
    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
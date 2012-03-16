open System

[<EntryPoint>]
let main args =

    let maxValue = 100;

    // the loop algo (no clue about the maths formula)
    let sum1 = Seq.fold (fun acc cur -> acc + cur*cur) 0 (seq { 1..maxValue })
    let sum2 = maxValue * (maxValue + 1) / 2
    let result = sum2 * sum2 - sum1

    printfn "%d" result

    let _ = Console.ReadKey true
    0
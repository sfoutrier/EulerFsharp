// http://projecteuler.net/problem=34

[<EntryPoint>]
let main args =

    let rec fact =
        function  1 -> 1 | 0 -> 1 | n -> n * fact (n - 1)

    let fact9 = fact 9
    let maxPossibleValue = 
        Seq.unfold (fun (x, i) -> if i * fact9 > x then Some (x * 10 + 9, (x * 10 + 9, i + 1)) else None) (9, 1)
        |> Seq.fold (fun acc last -> last) 0

    printfn "%d" maxPossibleValue

    let seqOfDigits =
        Seq.unfold (function 0 -> None | n -> Some (n % 10, n / 10))

    let curiousNumbers =
        Seq.filter
            (fun n -> n = Seq.sumBy fact (seqOfDigits n))
            {10..maxPossibleValue}

    //Seq.iter (printfn "%d") curiousNumbers
    printfn "%d" (Seq.sum curiousNumbers)

    let _ = System.Console.ReadKey true
    0
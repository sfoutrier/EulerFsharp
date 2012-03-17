
[<EntryPoint>]
let main args =
    let maxValue = 2000000
    let last = int (sqrt (float maxValue))

    let removeMultipleOf x = List.filter (fun y -> y % x <> 0 || y = x)
    let rec removeNotPrimes = function
        | index, curList when index < List.length curList ->
            removeNotPrimes (index + 1, removeMultipleOf ( List.nth curList index) curList)
        | _, endList -> endList

    let primes = removeNotPrimes (0, [ 2..last])
    
    let isDivisablePerAPrime x =
        not <| List.forall ( fun prime -> x % prime <> 0 ) primes

    let primes2 = Seq.choose (fun x -> if isDivisablePerAPrime x then None else Some(x)) (seq {last..maxValue}) |> Seq.map (fun x-> int64 x)

    printfn "%d" <| (int64 <| List.sum primes) + Seq.sum primes2

    let _ = System.Console.ReadKey true
    0
// http://projecteuler.net/problem=31

// the basic bruteforce algo ...
[<EntryPoint>]
let main args =

    let moneys = [| 1; 2; 5; 10; 20; 50; 100; 200 |]

    let moneysFrom n = seq {for i in moneys do if i >= n then yield i}

    let rec seqOfMoney moneyList curSum = 
        seq {
            let firstOrDefault = match moneyList with [] -> Array.min moneys | x::_ -> x
            for i in (moneysFrom firstOrDefault) do
                match i + curSum with
                | 200 -> yield (i :: moneyList)
                | x when x < 200 -> yield! (seqOfMoney (i :: moneyList) x)
                | _ -> ()
        }

    let allSeqOfMoney = seqOfMoney [] 0

    printfn "%d" (Seq.length allSeqOfMoney)

    let _ = System.Console.ReadKey true
    0
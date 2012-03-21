
[<EntryPoint>]
let main args =

    let maxNum = 5000000

    let sequence = function
        | n when n % 2 = 0 -> n/2
        | n -> 3 * n + 1
    let sequenceFromN = Seq.unfold (function 0 -> None | 1 -> Some (1 , 0) | n -> Some (n , sequence n) )

    let rec compute alreadyComputed x = match x, Map.tryFind x alreadyComputed with
                                        | 1, _ -> alreadyComputed, 1
                                        | x, None ->
                                            let (newComputed, chainLenth) = compute alreadyComputed (sequence x)
                                            (Map.add x (chainLenth + 1) alreadyComputed, chainLenth + 1)
                                        | x, Some(chainLenth) -> alreadyComputed, chainLenth
        
    //let rec seqOfSeqs = seq {}

    let seqOfSeqs = Seq.unfold
                        ( fun (alreadyComputed, x) -> 
                                if x >= maxNum then 
                                    None 
                                else
                                    let newComputed, chainLength = compute alreadyComputed x
                                    Some((x, chainLength), (newComputed, x + 1) ) )
                        ( Map.empty, 1 )

    let num, chainLength = Seq.maxBy snd seqOfSeqs

    printfn "%d" chainLength
    let chain = Seq.reduce (fun y z -> y + " -> " + z) (Seq.map (fun x -> x.ToString()) (sequenceFromN num))
    printfn "%s" chain

    let _ = System.Console.ReadKey true
    0
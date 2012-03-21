
[<EntryPoint>]
let main args =

    let maxNum = 200000

    let sequence = function
        | n when n % 2 = 0 -> n/2
        | n -> 3 * n + 1

    let sequenceFromN = Seq.unfold (function 0 -> None | 1 -> Some (1 , 0) | n -> Some (n , sequence n) )

    let listToFoundValue startingValue foundValues = 
        Seq.unfold (fun curValue -> match Map.tryFind curValue foundValues with 
                                    | Some( chainLength ) -> None
                                    | None -> Some( curValue, sequence curValue)
            ) startingValue
            |> Seq.fold (fun acc x -> x :: acc) []

    let computeFoundValuesAndLength value foundValues = 
        List.fold (fun (curLen, curFoundValues) curValue -> 
                    match curLen with
                        | 0 -> Map.find (sequence curValue) curFoundValues, curFoundValues
                        | _ -> (1 + curLen, Map.add curValue curLen curFoundValues) ) 
            (0, foundValues) (listToFoundValue value foundValues )

    let seqOfSeqs = Seq.unfold
                        ( fun (alreadyComputed, x) -> 
                                if x >= maxNum then 
                                    None 
                                else
                                    let chainLength, newComputed = computeFoundValuesAndLength x alreadyComputed
                                    Some((x, chainLength), (newComputed, x + 1) ) )
                        ( Map.add 1 1 Map.empty, 1 )

    let num, chainLength = Seq.maxBy snd seqOfSeqs

    printfn "%d" chainLength
    let chain = Seq.reduce (fun y z -> y + " -> " + z) (Seq.map (fun x -> x.ToString()) (sequenceFromN num))
    printfn "%s" chain

    let _ = System.Console.ReadKey true
    0
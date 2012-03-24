
[<EntryPoint>]
let main args =

    let maxNum = 1000000L

    let sequence = function
        | n when n < 0L -> raise (System.Exception "Negative")
        | n when n % 2L = 0L -> n/2L
        | n -> 3L * n + 1L

    let sequenceFromN = Seq.unfold (function 0L -> None | 1L -> Some (1L , 0L) | n -> Some (n , sequence n) )

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
                        ( function
                            | _, x when x <= 1L -> None 
                            | alreadyComputed, x ->
                                    let chainLength, newComputed = computeFoundValuesAndLength x alreadyComputed
                                    Some((x, chainLength), (newComputed, x - 1L) ) )
                        ( Map.add 1L 1 Map.empty, maxNum )

    let num, chainLength = Seq.maxBy snd seqOfSeqs

    printfn "%d" chainLength
    let chain = Seq.reduce (fun y z -> y + " -> " + z) (Seq.map (fun x -> x.ToString()) (sequenceFromN num))
    printfn "%s" chain

    let _ = System.Console.ReadKey true
    0
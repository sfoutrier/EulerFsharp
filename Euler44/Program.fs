// http://projecteuler.net/problem=44
open System

[<EntryPoint>]
let main args =

    let penta n = n*(3L*n-1L)/2L
    // n = x(3x-1)/2
    // <=>3x² - x -2n = 0
    //    => delta = 24n
    //    => x1 = - sqrt(24n)/6 (<0) | x2 = sqrt(24n)/6 (>0)
    let reversePenta n = (int64 << (Math.Round:float->float)) (sqrt (24. * (float n)) /6.)
    let isPenta n = n = (penta << reversePenta) n
    let pentasFromTo s n = Seq.unfold (function i when i = n -> None | i -> Some(penta i, i+1L) ) s

    let rec findPentaPair minPair n =
        let delta = function (a,b) -> a-b
        let min x y = match delta x < delta y with true -> x | false -> y
        let pentaN = penta n
        let startScanPosition = match minPair with Some(x) -> reversePenta (pentaN - (delta x)) | _ -> 1L
        match minPair with 
        // first let's stop when the gap have reached the min delta
        | Some(x) when delta x < pentaN - penta (n-1L)-> minPair
        | _ ->
            let newFoundPairs =
                // build pairs with our last client, start computing at where a smaller delta may be found
                Seq.map (fun p -> pentaN, p) (pentasFromTo startScanPosition n)
                // filter pairs matching our criteria
                |> Seq.filter (fun (a, b) -> isPenta (a-b) && isPenta (a+b))
                // keep the min one
                |> Seq.fold (fun state p -> match state with None -> Some(p) | Some(p2) -> Some(min p p2)) minPair
            findPentaPair newFoundPairs (n+1L)
    
    match findPentaPair None 1L with
    | Some(a,b) -> printfn "Result=%A with delta %d" (a,b) (a-b)

    printfn "Done"
    let _ = Console.ReadLine()
    0
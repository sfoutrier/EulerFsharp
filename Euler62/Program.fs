// https://projecteuler.net/problem=62
open System
open System.Numerics

[<EntryPoint>]
let main argv = 

    let sq x = x * x * x
    let keyOf (x:int) = BigInteger x |> sq |> string |> Seq.sort |> Seq.toArray |> String

    let cubes maxDigits =
        Seq.unfold
            (fun s ->
                match keyOf s with
                | k when k.Length <= maxDigits -> Some((k, s), s+1)
                | _ -> None
            ) 1
        |> Seq.fold
            (fun m (k, s) ->
                match Map.tryFind k m with
                | Some(l) -> Map.add k (s::l) m
                | None -> Map.add k [s] m
            ) Map.empty

    let findAnagrams occurences digits =
        cubes digits |> Map.toSeq
        //|> Seq.map snd// (fun (_,l) -> List.length l, List.rev l |> List.head )
        |> Seq.filter (fun (_,l) -> List.length l = occurences)
        |> Seq.map (fun (k,l) -> (k, List.rev l |> List.head |> BigInteger |> sq))

    Seq.iter (printfn "%A") (findAnagrams 5 12)

    printfn "Done"
    let _ = Console.ReadLine()

    0

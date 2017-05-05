// https://projecteuler.net/problem=63
open System

[<EntryPoint>]
let main argv =
    let matchingNumbers =
        {1..9} |> Seq.map bigint |> Seq.map
            (fun i-> Seq.unfold  (fun j ->
                match (string (pown i j)).Length with
                | l when l > j -> Some(false, j+1)
                | l when l = j -> Some(true, j+1)
                | _ -> None) 1)
        |> Seq.concat |> Seq.fold (fun c r -> match r with true -> c+1 | _ -> c) 0

    printfn "%i" matchingNumbers
    let _ = Console.ReadLine()

    0 // return an integer exit code

[<EntryPoint>]
let main args =
    let THRESHOLD = 95
    let cyphered = System.IO.File.ReadAllText("../../cipher1.txt").Split(',')
                    |> Array.map (System.Byte.Parse)
    let possibleKeys = seq {for a in 'a'..'z' do for b in 'a'..'z' do for c in 'a'..'z' -> [|byte a;byte b;byte c|]}
    let cycleKeys k = Seq.unfold (fun _ -> Some(k, ())) () |> Seq.concat
    let filterReadable s = Seq.filter (fun c -> (c>='A' && c<='z') || c=' ' || (c>='0' && c<='9')) s
    let readablePct s = (filterReadable s |> Seq.length) * 100 / (Array.length cyphered)
    let checksum s = Seq.fold (fun sum c -> int(c) + sum) 0 s

    Seq.map (fun k -> Seq.zip cyphered (cycleKeys k) |> Seq.map (fun (a,b) -> char(a ^^^ b)) |> fun s -> new string(Seq.toArray s)) possibleKeys
    |> Seq.filter (fun s -> (readablePct s) > THRESHOLD)
    |> Seq.iter (fun s -> printfn "%A %d" s (checksum s))
    
    printfn "Search ended"
    let _ = System.Console.ReadKey true
    0
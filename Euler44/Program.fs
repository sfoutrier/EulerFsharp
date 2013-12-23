// http://projecteuler.net/problem=44

[<EntryPoint>]
let main args =

    let p n = n * (3L * n - 1L) / 2L
    let p2 n =
        p (2L * n), p (2L * n + 1L)
    
    let pentagonalsSet =
        Seq.scan (fun set (p1, p2) -> Set.add p1 (Set.add p2 set)) Set.empty (Seq.initInfinite (int64>>p2))

    let seqOfPentasUnder (n, pentas) =
        seq {
            for j in 1L..(n-1L) do
                let x = p n
                let y = p j 
                if Set.contains (x+y) pentas && Set.contains (x-y) pentas then
                    yield x, y}

    let pentaWithSumAndDiff =
        Seq.zip (Seq.initInfinite (int64)) pentagonalsSet
        |> Seq.map seqOfPentasUnder
        |> Seq.concat

    Seq.iter (fun (x, y) -> printfn "%d %d" x y) pentaWithSumAndDiff

    let _ = System.Console.ReadKey true
    0

[<EntryPoint>]
let main args =
    // O(n^3) -> could be O(n^2) with some maths but I don't really care here since 1000 is not that big for n^3 ...
    let tuples = seq { 
        for i in 3..997 do
            for j in 2..i-1 do
                for k in 1..j-1 -> 
                    if (i*i = j*j + k*k) && ( i + j + k = 1000) then Some(k, j, i, i*j*k) else None }
    let a, b, c, result = Seq.pick (fun x -> x) tuples
    printfn "%d %d %d -> %d"  a b c result

    let _ = System.Console.ReadKey true
    0
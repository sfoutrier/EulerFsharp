
[<EntryPoint>]
let main args =

    let fibo = Seq.unfold (fun (a, b) -> Some(a + b, (b, a + b))) (1, 1)
    let fiboUnder4m = Seq.takeWhile (fun i -> i<4000000) fibo
    let result = Seq.fold (fun acc x -> if x%2=0 then acc + x else acc) 0 fiboUnder4m

    printfn "%d" result

    let toto = System.Console.ReadLine()
    System.Console.WriteLine toto
    0
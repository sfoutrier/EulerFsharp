
[<EntryPoint>]
let main args =

    let t n = n * (n + 1L) / 2L
    let p n = n * (3L * n - 1L) / 2L
    let h n = n * (2L * n - 1L)

    let inc n = n + 1L

    let f = fun (a, b, c) i ->
        if t a < p b && t a < h c then
            (inc a, b, c)
        else if p b  < h c then
            (a, inc b, c)
        else 
            (a, b, inc c)

    let sequence = Seq.scan f (286L, 166L, 144L) (Seq.initInfinite (fun i -> i))
    let result = Seq.find (fun (a, b, c) -> t a = p b && p b = h c) sequence

    match result with (a, b, c) -> printfn "t%O = p%O = h%O = %O" a b c (t a)
    let _ = System.Console.ReadKey true
    0
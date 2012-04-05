// http://projecteuler.net/problem=32

[<EntryPoint>]
let main args =

    let allFractions =
        seq { for a in {10..99} do
                for b in {a..99} -> a, b }
    
    let rec pgcd a b =
        if a < b then
            pgcd b a
        else
            match a % b with
            | 0 -> b
            | c -> pgcd b c

    let simplifyedFraction (a, b) =
        let c = pgcd a b
        a / c, b / c

    let removeIdentical (a, b) =
        if a / 10 = b / 10 then
            Some (a % 10, b % 10)
        else if a / 10 = b % 10 then
            Some (a % 10, b / 10)
        else if a  % 10 = b / 10 then
            Some (a / 10, b % 10)
        else if a % 10 = b % 10 then
            Some (a / 10, b / 10)
        else
            None
    
    let isCuriousFraction (a, b) =
        if a >= b || (a % 10 = 0 && b % 10 = 0) then
            false
        else
            match removeIdentical (a, b) with
            | None -> false
            | Some (i, j) ->
                let k, l = simplifyedFraction (a, b)
                Seq.exists (fun x -> i = k * x && j = l * x) {1..9}

    (*Seq.iter (fun (a, b) ->
                match removeIdentical (a, b) with
                | Some (c, d) -> printfn "%d / %d = %d / %d" a b c d
                | None -> () )
            (Seq.filter isCuriousFraction allFractions)*)

    let resN, resD =
        Seq.reduce 
            (fun (x, y) (z, t) -> x * z, y * t) 
            (Seq.filter isCuriousFraction allFractions)
        |> simplifyedFraction
        
    printfn "%d / %d " resN resD

    let _ = System.Console.ReadKey true
    0
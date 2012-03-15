
[<EntryPoint>]
let main args =

    let threeDigits = seq {for x in 0..899 -> (999-x, seq { for y in 0..899 -> 999-y }) }

    let arrayOfDigit =
        Seq.unfold (function
            | 0 -> None
            | x -> Some(x%10, x/10)) 
        >> Seq.toArray

    let isPalindrome x =
        let xAsArray = arrayOfDigit x
        Array.forall2 (=) xAsArray (Array.rev xAsArray)

    let noneTo0 = function 
        | Some(x) -> x
        | None -> 0 

    let maxOfASeq (x, seqOfY) = 
        (x, noneTo0 <| Seq.tryFind (fun y -> isPalindrome (x * y)) seqOfY )

    let greater (x, y) (z, t) = 
        if(x*y >= z*t) then (x, y) else (z, t)

    let folder acc (x, seqOfY) =
        greater acc <| maxOfASeq (x, seqOfY)

    let max = Seq.fold folder (0, 0) threeDigits

    printfn "%d * %d = %d" (fst max) (snd max) ((fst max) * (snd max))

    let toto = System.Console.ReadLine()
    System.Console.WriteLine toto
    0
[<EntryPoint>]
let main args =
    let valToCompute = 600851475143L
    let maxValue = System.Convert.ToInt64 (sqrt 600851475143.)

    let rec divWhileDivisible value x =
        if(value%x=0L) then 
            divWhileDivisible (value/x) x 
        else 
            value

    let folder (curValue, maxDiv) cur =
        if(curValue%cur=0L) then 
            divWhileDivisible curValue cur, cur
        else 
            curValue, maxDiv

    let result = Seq.fold folder (valToCompute, 1L) {2L..maxValue}

    System.Console.WriteLine result

    let toto = System.Console.ReadLine()
    System.Console.WriteLine toto
    0

[<EntryPoint>]
let main args =
    let maxNum = 10001
    //let maxNum = 6

    let infiniteSeq = Seq.initInfinite (fun x -> x + 2)

    let rec nextPrime (firstPrimes, index) n =
        // should be optimized to run only on primes > sqrt(index), not sure of the gain
        if (List.exists (fun x -> index % x = 0) firstPrimes) then
            nextPrime (firstPrimes, index + 1) n
        else
            (index::firstPrimes, index)

    let primes = Seq.scan nextPrime ([], 2) infiniteSeq
    let firstPrimes = Seq.nth maxNum primes

    printfn "%d" (snd firstPrimes)

    let _ = System.Console.ReadKey true
    0
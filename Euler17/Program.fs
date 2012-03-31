// from http://en.wikipedia.org/wiki/English_numerals
let numbers = Map [
                    (1, "one");
                    (2, "two");
                    (3, "three");
                    (4, "four");
                    (5, "five");
                    (6, "six");
                    (7, "seven");
                    (8, "eight");
                    (9, "nine");
                    (10, "ten");
                    (11, "eleven");
                    (12, "twelve");
                    (13, "thirteen");
                    (14, "fourteen");
                    (15, "fifteen");
                    (16, "sixteen");
                    (17, "seventeen");
                    (18, "eighteen");
                    (19, "nineteen");
                    (20, "twenty");
                    (30, "thirty");
                    (40, "forty");
                    (50, "fifty");
                    (60, "sixty");
                    (70, "seventy");
                    (80, "eighty");
                    (90, "ninety");
                    (100, "hundred");
                    (1000, "thousand");
                ]

[<EntryPoint>]
let main args =
    let numbersUnder100 = Map (seq {
                            for i in 1..99 -> 
                                match Map.tryFind i numbers with
                                | Some(value) -> i, [value]
                                | None -> 
                                    match Map.tryFind (i%10) numbers with
                                    | Some(digit) -> i, [ Map.find (i - i%10) numbers; digit]
                                    | None -> i, [ Map.find (i - i%10) numbers]
                            })

    let numbersUnder1000 = seq { for i in 1..999 ->
                                        match (i / 100, i % 100) with
                                        | 0, _ -> Map.find i numbersUnder100
                                        | x, 0 -> [Map.find x numbers; Map.find 100 numbers]
                                        | x, y -> Map.find x numbers :: Map.find 100 numbers :: "and" :: Map.find y numbersUnder100
                            }

    //Seq.iter (fun x-> printfn "%A %d" x (List.sumBy (String.length) x)) numbersUnder1000

    let numbersTo1000 = Seq.append numbersUnder1000 [[Map.find 1 numbers;Map.find 1000 numbers]]
    printfn "%d" (Seq.sumBy (List.sumBy (String.length)) numbersTo1000) 

    let _ = System.Console.ReadKey true
    0
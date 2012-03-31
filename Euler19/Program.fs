
[<EntryPoint>]
let main args =
    
    let firstOfMonths = Seq.unfold (function
                                    | _, _, year when year > 2000
                                        -> None
                                    | day, 2, year when year % 4 = 0 && not ((year % 100 = 0) && not (year % 400 = 0))
                                        -> Some((day, 2, year), ((day + 29) % 7, 3, year))
                                    | day, 2, year
                                        -> Some((day, 2, year), ((day + 28) % 7, 3, year))
                                    | day, month, year when month = 4 || month = 6 || month = 9 || month = 11
                                        -> Some((day, month, year), ((day + 30) % 7, month + 1, year))
                                    | day, month, year when month = 12
                                        -> Some((day, month, year), ((day + 31) % 7, 1, year + 1))
                                    | day, month, year
                                        -> Some((day, month, year), ((day + 31) % 7, month + 1, year) ))
                                        (1, 1, 1900)

    let result = Seq.sumBy (function 0,_,year when year >= 1901 -> 1 | _,_,_ -> 0) firstOfMonths
    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
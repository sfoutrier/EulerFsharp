// http://projecteuler.net/problem=22
open System.IO;

[<EntryPoint>]
let main args =
    use namesFile = File.OpenText "../../names.txt"
    let names = namesFile.ReadToEnd().Split ',' 
                    |> Array.map (fun (x:string) -> x.Trim '"')
                    |> Array.sort
                    
    let score = Seq.sumBy (fun x -> (int64)x - (int64)'A' + 1L)

    let totalScore = Array.fold (fun (i, curScore) name -> (i + 1L, curScore + i * score name)) (1L, 0L) names |> snd

    printfn "%d" totalScore
    let _ = System.Console.ReadKey true
    0
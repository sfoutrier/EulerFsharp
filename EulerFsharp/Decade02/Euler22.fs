// http://projecteuler.net/problem=22
namespace Decade02
open System.IO;

module Euler22 =

    let main args =
        use namesFile = File.OpenText "../../names22.txt"
        let names = namesFile.ReadToEnd().Split ','
                        |> Array.map (fun (x:string) -> x.Trim '"')
                        |> Array.sort

        let score = Seq.sumBy (fun x -> (int64)x - (int64)'A' + 1L)

        let totalScore = Array.fold (fun (i, curScore) name -> (i + 1L, curScore + i * score name)) (1L, 0L) names |> snd

        printfn "%d" totalScore
        let _ = System.Console.ReadKey true
        0
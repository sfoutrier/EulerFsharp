// http://projecteuler.net/problem=42
namespace Decade02
open System.IO;

module Euler42 =
    let main args =

        use wordsFile = File.OpenText "../../words42.txt"
        let words = Seq.unfold (fun (file:StreamReader) ->
                                    match file.EndOfStream with
                                    | false -> Some(file.ReadLine().Split ',' |> Array.map (fun x -> x.Trim '"'), file)
                                    | true -> None) wordsFile
                    |> Seq.concat

        let isTriangleWord word =
            let wordValue = Seq.sumBy (fun c -> (int)c - (int)'A' + 1) word
            let sqrtOfValue = (int)(sqrt(((double)(wordValue * 2))))
            2 * wordValue = sqrtOfValue * (sqrtOfValue + 1)

        let triangleWords =
            Seq.filter isTriangleWord words

        printfn "%d" (Seq.length triangleWords)
        //Seq.iter (printfn "%s") triangleWords

        let _ = System.Console.ReadKey true
        0
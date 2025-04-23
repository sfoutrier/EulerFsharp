namespace Decade01
open System.IO;

module Euler18 =
    let main args =
        use triangleFile = File.OpenText "../../triangle18"
        let triangle = Seq.unfold (fun (file:StreamReader) ->
                                    match file.EndOfStream with
                                    | false -> Some(file.ReadLine().Split ' ' |> Array.map System.Int32.Parse, file)
                                    | true -> None) triangleFile |> Seq.toList

        let rec paths position lines currentPath =
            seq {
                match lines with
                | [] ->
                    yield currentPath
                | x::tail ->
                    yield! paths position tail (Array.get x position :: currentPath)
                    yield! paths (position + 1) tail (Array.get x (position + 1) :: currentPath)
            }

        let allPaths = paths 0 (List.tail triangle) [(Seq.head (List.head triangle))]
        let maxPath = Seq.maxBy List.sum allPaths
        printfn "%d : %A" (List.sum maxPath) maxPath

        let _ = System.Console.ReadKey true
        0
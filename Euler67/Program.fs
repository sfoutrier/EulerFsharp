open System.IO;

[<EntryPoint>]
let main args =
    use triangleFile = File.OpenText "../../triangle.txt"
    let triangle = Seq.unfold (fun (file:StreamReader) -> 
                                match file.EndOfStream with
                                | false -> Some(file.ReadLine().Split ' ' |> Array.map System.Int32.Parse, file)
                                | true -> None) triangleFile 
                            |> Seq.toList 
    
    let reducer x y = 
        Seq.map (fun (a,b) -> a + Array.max b) 
            (Seq.zip (Array.toSeq x) (Seq.windowed 2 (Array.toSeq y)))
            |> Seq.toArray

    let result = List.reduceBack reducer triangle |> Seq.head

    printfn "%d" result

    let _ = System.Console.ReadKey true
    0
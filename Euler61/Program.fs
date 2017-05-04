// https://projecteuler.net/problem=61
open System

let polygonals formula =
    Seq.unfold 
        (fun n ->
        match formula n with
        | v when v < 10000 -> Some(v, n+1)
        | _ -> None) 1
    |> Seq.filter ((<) 999) |> Seq.toArray

type Polygonal =
    | Triangle
    | Square
    | Pentagonal
    | Hexagonal
    | Heptagonal
    | Octagonal

let allPolygonals =
    Map[( Triangle,     polygonals (fun n -> n*(n+1)/2) )
        ( Square,       polygonals (fun n -> n*n) )
        ( Pentagonal,   polygonals (fun n -> n * (3*n-1)/2))
        ( Hexagonal,    polygonals (fun n -> n* (2*n-1)))
        ( Heptagonal,   polygonals (fun n -> n*(5*n-3)/2))
        ( Octagonal,    polygonals (fun n -> n *(3*n-2)) )]

let nextLevel curSeq =
    match curSeq with
    | [] ->
        // bootstrap with a random polygonal
        let (p,v) = Map.toSeq allPolygonals |> Seq.head
        Seq.map (fun i -> [i, p]) v
    | (head,_)::_ ->
        seq {
            for polygonal in allPolygonals |> Map.toSeq |> Seq.map fst do
            match List.exists (function _,p when p = polygonal -> true | _ -> false) curSeq with
            // already have this shape, skipping
            | true -> ()
            | false ->
                for i in (Map.find polygonal allPolygonals) do
                match head / 100 = i % 100 with
                // doesn't continue the cycle, drop it
                | false -> ()
                | true -> yield (i,polygonal)::curSeq
        }

let rec findSequenceOf (curSeq:(int*Polygonal)list) depth =
    match depth with
    | 0 ->
        match (List.head curSeq |> fst) / 100 = (List.rev curSeq |> List.head |> fst) % 100 with
        | true -> Seq.singleton curSeq
        | false -> Seq.empty
    | _ ->
        seq {
            for nextSeq in nextLevel curSeq do
                yield findSequenceOf nextSeq (depth - 1)
        } |> Seq.concat

[<EntryPoint>]
let main argv =
    printfn "Start computing cycle"
    Seq.iter (fun r -> printfn "%A -> Sum=%i" r (List.sumBy fst r)) (findSequenceOf [] 6)

    printfn "Done"
    let _ = Console.ReadLine()
    0 // return an integer exit code

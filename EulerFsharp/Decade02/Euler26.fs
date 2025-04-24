// http://projecteuler.net/problem=26
namespace Decade02

module Euler26 =

    let main args =

        // I just keep the rest of the succesive divisions (to detect cycles on rest repetition)
        // stop after finding a collision
        let restOfNth n =
            Seq.unfold (fun (rest, foundRests) ->
                            match rest, Set.contains rest foundRests with
                            | 0, _ -> None
                            | _, false -> Some (rest*10%n, (rest*10%n, Set.add rest foundRests))
                            | _, true -> Some (rest*10%n, (0, Set.empty))
                        ) (1, Set.empty)

        let cycleLength n =
            match restOfNth n |> Seq.toList |> List.rev with
            | head::tail -> List.tryFindIndex (fun x -> head = x) tail
            | [] -> None

        let cycleLengthTo1000 = Seq.map (
                                    fun i ->
                                        match cycleLength i with
                                        | None -> (i, 0)
                                        | Some x -> (i, x + 1)
                                ) {1..1000}

        let i, nbCycles = Seq.maxBy snd cycleLengthTo1000
        printfn "%d -> %d" i nbCycles

        let _ = System.Console.ReadKey true
        0
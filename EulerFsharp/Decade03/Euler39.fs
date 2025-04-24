// http://projecteuler.net/problem=39

// the basic brute force, an equation should soulve this with a better complexity
namespace Decade03

module Euler39 =
    let main args =

        let hasRightAngle (x, y, z) =
            x * x + y * y = z * z
        let isOrdered (x, y, z) =
            (x <= y) && (y < z)

        let trianglesOfP p =
            seq {
                for x in (p/2)..(p-2) do
                    for y in 1..(x-1) do
                        yield (p - x - y, y, x) }
            |> Seq.filter isOrdered
            |> Seq.filter hasRightAngle

        let maxP =
            Seq.maxBy (trianglesOfP >> Seq.length) {1..1000}

        printfn "%d" maxP

        let _ = System.Console.ReadKey true
        0
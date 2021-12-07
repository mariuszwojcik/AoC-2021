open System
open System.IO




let findCheapestHPos costCalc input = 

    let calculateFuelCost p (a : int[]) =
        a 
        |> Array.map(fun i -> (i - p) |> Math.Abs |> costCalc)
        |> Array.sum

    let max = input |> Array.max

    [0..max]
    |> List.fold(fun c v -> 
        let c' = calculateFuelCost v input
        if c' < c then c' else c
        ) Int32.MaxValue



//let FName = "Day07/sample.txt"
let FName = "Day07/input.txt"

let input = 
    File.ReadAllText(FName).Split(",")
    |> Array.map int

// part 1
input |> findCheapestHPos (fun i -> i)

// part 2
input |> findCheapestHPos (fun i -> [1..i] |> List.sum)

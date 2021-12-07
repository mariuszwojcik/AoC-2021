open System;
open System.IO;


let countFish iterations fishInput =

    let days = Array.init 9 (fun _ -> uint64 0)

    let addFishToDay (fc : uint64) (d : int) (a : uint64[]) =
        a |> Array.mapi(fun i v -> if i = d then v+fc else v)

    let initialState = fishInput |> Array.fold(fun s v -> addFishToDay 1UL v s) days

    [|0..iterations-1|]
    |> Array.fold(fun (s : uint64[]) i -> 
            let t = i % days.Length
            let d = (t + 7) % days.Length
            let fc = s.[t]
            let r = addFishToDay fc d s

            r
        ) initialState
    |> Array.sum




let FName = "Day06/input.txt"

let input = 
    File.ReadAllText(FName).Split(",")
    |> Array.map int

// part 1
input |> countFish 80

// part 2
input |> countFish 256
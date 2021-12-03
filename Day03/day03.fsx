open System;
open System.IO;

//let fname, bitsToConsider = @"Day03/sample.txt", 5
let fname, bitsToConsider = @"Day03/input.txt", 12

let showMask (v : UInt16) = Convert.ToString(Convert.ToInt64(v), 2)

let mostCommonBit p input =
    let mask = ~~~(System.UInt16.MaxValue >>> 1) >>> (16-p)

    input
    |> Array.map(fun v -> v &&& mask >>> (p-1))
    |> Array.groupBy id
    |> Array.map(fun (a,b) -> a, b |> Array.length)
    |> Array.groupBy(fun (a,b) -> b)
    |> Array.sortByDescending(fun (a,_) -> a)
    |> Array.head
    |> snd
    |> Array.map(fun (a,b) -> a)


let calculateGammaRate bitsToConsider input =
    [1..bitsToConsider]
    |> List.rev
    |> List.map(fun p -> (mostCommonBit p input).[0] <<< (p-1))
    |> List.sum
    
let calculateEpsilonRate bitsToConsider input =
    [1..bitsToConsider]
    |> List.rev
    |> List.map(fun p -> ((mostCommonBit p input).[0] ^^^ 1us) <<< (p-1))
    |> List.sum

let calculatePowerConsumption bitsToConsider input =
    let g = calculateGammaRate bitsToConsider input |> uint
    let e = calculateEpsilonRate bitsToConsider input |> uint
    g * e



let rec calculateOGenRating p input =
    let mask = ~~~(System.UInt16.MaxValue >>> 1) >>> (16-p)
    
    let mcb = input |> mostCommonBit p
    let input' = 
        match mcb with
        | [| 1us |]  -> input |> Array.filter(fun v -> v &&& mask > 0us)
        | [| 0us |] -> input |> Array.filter(fun v -> v &&& mask = 0us)
        | [| 0us; 1us |]  -> input |> Array.filter(fun v -> v &&& mask > 0us)

        | x -> failwithf "%A" x

    if input'.Length > 1 && p > 0 then
        calculateOGenRating (p-1) input'
    else
        input' |> Array.head

let rec calculateCO2ScrubberRating p input =
    let mask = ~~~(System.UInt16.MaxValue >>> 1) >>> (16-p)
    
    let mcb = input |> mostCommonBit p
    let input' = 
        match mcb with
        | [| 1us |] -> input |> Array.filter(fun v -> v &&& mask = 0us)
        | [| 0us |] -> input |> Array.filter(fun v -> v &&& mask <> 0us)
        | [| 1us; 0us |] -> input |> Array.filter(fun v -> v &&& mask = 0us)

        | x -> failwithf "%A" x

    if input'.Length > 1 && p > 0 then
        calculateCO2ScrubberRating (p-1) input'
    else
        input' |> Array.head

let calculateLifeSupportRating input =
    let o = calculateOGenRating bitsToConsider input |> uint
    let co2 = calculateCO2ScrubberRating bitsToConsider input |> uint
    o * co2


let input = File.ReadAllLines(fname) |> Array.map(fun s -> Convert.ToUInt16(s, 2))

// part 1
input |> calculatePowerConsumption bitsToConsider

// part 2
input |> calculateLifeSupportRating

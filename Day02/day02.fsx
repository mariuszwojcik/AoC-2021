open System;
open System.IO;

//let fname = @"Day02/sample.txt"
let fname = @"Day02/input.txt"

type Move =
    | Forward of d : int
    | Down of d : int
    | Up of d : int
and Coords = { HPos : int; Depth : int }

let parse (s : string) : Move =
    let p = s.Split(" ")
    match p with
    | [| "forward"; a |] -> Forward(d= int a)
    | [| "up"; a |] -> Up(d= int a)
    | [| "down"; a |] -> Down(d= int a)
    | _ -> failwithf "Parse error: %A" p

let move m coords =
    match m with
    | Forward f -> { coords with HPos = coords.HPos + f }
    | Up f -> { coords with Depth = coords.Depth - f }
    | Down f -> { coords with Depth = coords.Depth + f }



// Part 1
File.ReadAllLines(fname)
|> Array.map parse
|> Array.fold(fun c m -> move m c) { HPos = 0; Depth = 0}
|> fun c -> c.HPos * c.Depth


// Part 2
type CoordsWithAim = { HPos : int; Depth : int; Aim : int }

let moveWithAim m coords =
    match m with
    | Forward f -> { coords with CoordsWithAim.HPos = coords.HPos + f; Depth = coords.Depth + (coords.Aim * f) }
    | Up f -> { coords with Aim = coords.Aim - f }
    | Down f -> { coords with Aim  = coords.Aim + f }



// Part 1
File.ReadAllLines(fname)
|> Array.map parse
|> Array.fold(fun c m -> moveWithAim m c) { HPos = 0; Depth = 0; Aim = 0}
|> fun c -> c.HPos * c.Depth

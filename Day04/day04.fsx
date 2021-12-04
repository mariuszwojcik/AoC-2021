open System;
open System.IO;

//let FName = @"Day04/sample.txt"
let FName = @"Day04/input.txt"

type BoardCell =
    | Unmarked of int
    | Marked of int

and  Board = {
    Cells : BoardCell[][]
} with 
    member this.getCol c = [|0..4|] |> Array.map(fun i -> this.Cells.[i].[c])
    member this.Columns  = [|0..4|] |> Array.map this.getCol


let loadBoard (a : string[]) =
    let parseLine (s : string) =
        s.Split(" ") 
        |> Array.filter(fun s -> s <> "") 
        |> Array.map int 
        |> Array.map(fun i -> Unmarked i)

    a
    |> Array.skip 1
    |> Array.map parseLine
    |> fun a -> { Cells = a}

let isMarked c =
        match c with
        | Unmarked _ -> false
        | Marked _ -> true


let getCellVal c =
    match c with
    | Marked v -> v
    | Unmarked v -> v

let markCell d (board : Board) =
    let cells = 
        board.Cells
        |> Array.map(fun a -> a |> Array.map(fun a' -> if a' = Unmarked d then Marked d else a'))
    {board with Cells = cells}

let isWinner (board : Board) = //board.isWinner
    let a = 
        board.Cells
        |> Array.tryFind(fun a -> a |> Array.filter (isMarked >> not) |> Array.length = 0)
        |> Option.isSome
    let b = 
        board.Columns
        |> Array.tryFind(fun a -> a |> Array.filter (isMarked >> not) |> Array.length = 0)
        |> Option.isSome

    a || b


let calculateScore (n, board) =
    let sum =
        board.Cells
        |> Array.collect(fun a -> a)
        |> Array.filter (isMarked >> not)
        |> Array.map getCellVal
        |> Array.sum
    sum * n

let LoadInput fname =
    let lines = File.ReadAllLines(fname)
    let numbers = lines.[0].Split(",") |> Array.map int
    let boards = 
        lines 
        |> Array.skip(1) 
        |> Array.chunkBySize 6
        |> Array.map loadBoard

    boards, numbers


let input = LoadInput FName


// part 1
let rec playPart1 (boards, numbers) =
    
    if numbers = [||] then failwith "no winner found!"
    
    let n = numbers |> Array.head
    let numbers' = numbers |> Array.tail
    let boards' =
        boards
        |> Array.map(markCell n)

    let winner =
        boards'
        |> Array.tryFind isWinner
    
    match winner with
    | None -> playPart1 (boards', numbers')
    | Some b ->
        n, b


input
|> playPart1
|> calculateScore


// part 2
let rec playPart2 (boards, numbers) =
    
    if numbers = [||] then failwith "no winner found!"
    
    let n = numbers |> Array.head
    let numbers' = numbers |> Array.tail
    let boards' =
        boards
        |> Array.map(markCell n)
        |> Array.filter (isWinner >> not)

    match boards' with
    | [| |] -> n, (markCell n boards.[0])
    | _ -> playPart2 (boards', numbers')


input
|> playPart2
|> calculateScore

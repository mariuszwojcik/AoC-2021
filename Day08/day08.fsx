open System;
open System.IO;

//let FName = "Day08/sample.txt"
let FName = "Day08/input.txt"

let categorisePattern (s : string) =
    match s.Length with
    | 2 -> [| 1 |]
    | 3 -> [| 7 |]
    | 4 -> [| 4 |]
    | 5 -> [| 2; 3; 5|]
    | 6 -> [| 0; 6; 9 |]
    | 7 -> [| 8 |]
    | _ -> failwithf "Invalid pattern: %s" s

// part 1 (383)
File.ReadAllLines(FName)
|> Array.map(fun s -> s.Split(" | ").[1])
|> Array.collect(fun s -> s.Split(" "))
|> Array.filter(fun s -> categorisePattern s |> Array.length = 1)
|> Array.length

type Entry = { patterns : string[] ; output : string[] }

type Display = {
    Segments : Set<char>[]
} with 
    member this.allLetters = this.Segments |> Array.reduce(fun s s' -> s + s')
    member this.getSegments a = a |> Array.map(fun i -> this.Segments.[i]) |> Array.reduce(fun s s' -> s + s')
    member this.Digits = [|
        this.getSegments [| 0;1;2;4;5;6|]
        this.getSegments [| 2;5|]
        this.getSegments [| 0;2;3;4;6|]
        this.getSegments [| 0;2;3;5;6 |]
        this.getSegments [| 1;2;3;5|]
        this.getSegments [| 0;1;3;5;6|]
        this.getSegments [| 0;1;3;4;5;6|]
        this.getSegments [| 0;2;5|]
        this.getSegments [| 0..6|]
        this.getSegments [| 0;1;2;3;5;6|]
    |]


let SetSegment idx d dispay =
    let s = 
        dispay.Segments
        |> Array.mapi (fun i s -> if i = idx-1 then d else s)
    {dispay with Segments = s}


let findSegments entry =
    let p = entry.patterns |> Array.map(fun s -> s, categorisePattern s)
    let p1 = p |> Array.find(fun (p,n) -> n = [| 1 |]) |> fst |> Seq.map id |> Set.ofSeq
    let p7 = p |> Array.find(fun (p,n) -> n = [| 7 |]) |> fst |> Seq.map id |> Set.ofSeq
    let p4 = p |> Array.find(fun (p,n) -> n = [| 4 |]) |> fst |> Seq.map id |> Set.ofSeq
    let p235 = p |> Array.filter(fun (p,n) -> n = [| 2;3;5 |]) |> Array.map(fun (s,_) -> s |> Seq.map id |> Set.ofSeq)

    let d = 
        { Segments = Array.init 7 (fun _ -> Set<char>[]) }
        |> SetSegment 1 (p7-p1)
        |> SetSegment 2 (p4-p1)
        |> SetSegment 3 p1
        |> SetSegment 4 (p4-p1)
        |> SetSegment 6 p1


    let p3 = p235 |> Array.find(fun s -> p7 - s = set [])

    let s7 = p3 - p4 - p7
    let s2 = d.Segments.[3] - p3
    let s4 = d.Segments.[1] - s2

    let d' =
        d
        |> SetSegment 7 s7
        |> SetSegment 4 s4
        |> SetSegment 2 s2


    let p2 = p235 |> Array.find(fun s -> s - d'.allLetters <> Set.empty)

    let s5 = p2 - d'.allLetters
    let s6 = d'.Segments.[5] - p2
    let s3 = d'.Segments.[2] - s6

    d'
    |> SetSegment 5 s5
    |> SetSegment 6 s6
    |> SetSegment 3 s3

let decode (ds : Display) output = 
    let o = output |> Seq.map id |> Set.ofSeq
    categorisePattern output
    |> Array.find(fun i -> ds.Digits.[i] = o)


let parse (s : string) =
    let p = s.Split(" ")
    { 
        patterns = p.[..9]
        output = p.[11..]
    }

let findOutputValue s =
    let e = s |> parse

    let ds = (findSegments e)

    e.output
    |> Array.map(decode ds)
    |> Array.mapi(fun i d -> d * (int <| Math.Pow(10., float (3-i))))
    |> Array.sum


// part 2
File.ReadAllLines(FName)
|> Array.map findOutputValue
|> Array.map int64
|> Array.sum


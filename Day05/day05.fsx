open System
open System.Text.RegularExpressions
open System.IO

type Coords = { x: int; y: int }
type Direction = | Horizontal | Vertical | Diagonal
type Line = 
    { c1 : Coords; c2: Coords }
    with member this.direction = 
            if this.c1.x = this.c2.x then Vertical
            else if this.c1.y = this.c2.y then Horizontal
            else Diagonal


let parse (s :string) =
    let m = Regex.Match(s, "(\d*),(\d*) -> (\d*),(\d*)")
    { 
        c1 = { 
            x = Int32.Parse(m.Groups.[1].Value) 
            y = Int32.Parse(m.Groups.[2].Value) 
        }
        c2 = {
            x = Int32.Parse(m.Groups.[3].Value) 
            y = Int32.Parse(m.Groups.[4].Value) 
        }
    }



let expand (l : Line) =
    match l.direction with
    | Horizontal ->
        let a = Math.Min(l.c1.x, l.c2.x)
        let b = Math.Max(l.c1.x, l.c2.x)
        [| a..b |]
        |> Array.scan(fun s v -> {s with x = v }) l.c1
        |> Array.skip 1
    | Vertical ->
        let a = Math.Min(l.c1.y, l.c2.y)
        let b = Math.Max(l.c1.y, l.c2.y)
        [| a..b |]
        |> Array.scan(fun s v -> {s with y = v }) l.c1
        |> Array.skip 1
    | Diagonal when l.c1.x < l.c2.x && l.c1.y < l.c2.y ->
        [| l.c1.y .. l.c2.y |]
        |> Array.zip [| l.c1.x .. l.c2.x |]
        |> Array.scan(fun s (x,y) -> {s with x = x; y = y }) l.c1
        |> Array.skip 1
    | Diagonal when l.c1.x < l.c2.x && l.c1.y > l.c2.y ->
        [| l.c2.y .. l.c1.y |] |> Array.rev
        |> Array.zip [| l.c1.x .. l.c2.x |]
        |> Array.scan(fun s (x,y) -> {s with x = x; y = y }) l.c1
        |> Array.skip 1
    | Diagonal when l.c1.x > l.c2.x && l.c1.y < l.c2.y ->
        [| l.c1.y .. l.c2.y |]
        |> Array.zip ([| l.c2.x .. l.c1.x |] |> Array.rev)
        |> Array.scan(fun s (x,y) -> {s with x = x; y = y }) l.c1
        |> Array.skip 1
    | Diagonal ->
        [| l.c2.y .. l.c1.y |] |> Array.rev
        |> Array.zip ([| l.c2.x .. l.c1.x |] |> Array.rev)
        |> Array.scan(fun s (x,y) -> {s with x = x; y = y }) l.c1
        |> Array.skip 1


let FName = "Day05/input.txt"

let lines =
    File.ReadAllLines(FName)
    |> Array.map parse
    

let countOverlappedPoints lines = 
    lines
    |> Array.collect expand
    |> Array.groupBy id
    |> Array.map(fun (g, i) -> g, i.Length)
    |> Array.filter(fun (_, c) -> c > 1)
    |> Array.map(fun (c, _) -> c)
    |> Array.length

// part 1

lines
|> Array.filter(fun l -> l.direction = Diagonal |> not)
|> countOverlappedPoints


// part 2
lines
|> countOverlappedPoints

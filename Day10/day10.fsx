open System
open System.IO

// let FName = "Day10/sample.txt"
let FName = "Day10/input.txt"

let isLineIncomplete (line : string) =
    line.Length % 2 = 1

let isClosingChunk (c : char) =
    c = ')' || c = ']' || c = '}' || c = '>'

let getComplementingTag c =
    match c with
    | '(' -> ')'
    | '{' -> '}'
    | '[' -> ']'
    | '<' -> '>'
    | _ -> failwithf "Wrong chunk: %A" c

let isComplementingTag c1 c2 =
    c2 = getComplementingTag c1

let scoreSyntaxError c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwithf "Wrong chunk: %A" c

let isLineCorrupted (line : string) =
    
    line
    |> Seq.map id
    |> Seq.toArray
    |> Seq.fold(fun (corrupted,s) i ->
        match corrupted with
        | true -> (corrupted, s)
        | false ->
            if isClosingChunk i then
                let h = s |> List.head
                if isComplementingTag h i then
                    false, s |> List.tail
                else
                    true, [h; i]
            else
                false, [i] @ s
    ) (false, ([] : char list))
    
    



// part 1 (271245)
File.ReadAllLines(FName)
|> Array.map(isLineCorrupted)
|> Array.filter fst
|> Array.map(fun (_,l) -> l.[1])
|> Array.map scoreSyntaxError
|> Array.sum


// part 2 (169394671)
let getMissingChunks (line : string) =
    
    line
    |> Seq.map id
    |> Seq.toArray
    |> Seq.fold(fun (corrupted,s) i ->
        match corrupted with
        | true -> (corrupted, s)
        | false ->
            if isClosingChunk i then
                let h = s |> List.head
                if isComplementingTag h i then
                    false, s |> List.tail
                else
                    true, s
            else
                false, [i] @ s
    ) (false, ([] : char list))
    |> snd
    |> List.map getComplementingTag

let scoreAutocompleteChunk c =
    match c with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwithf "Wrong chunk: %A" c



let scoreCompletion (a : char list) =
    a
    |> List.map scoreAutocompleteChunk
    |> List.fold (fun s i -> s * 5L + (int64 i)) 0L


File.ReadAllLines(FName)
|> Array.filter(isLineCorrupted >> fst >> not)
|> Array.map getMissingChunks
|> Array.map scoreCompletion
|> Array.sort
|> fun a -> a.[a.Length / 2]

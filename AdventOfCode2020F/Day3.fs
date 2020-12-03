module Day3

type Movement = { line: string; Y: int; TreesCounter: int }

let skipLines down = function
    | (i, _) when i % down = 0 -> true
    | _ -> false
    
let GoToboggan map right down =    
    let result =
        map
        |> Seq.indexed
        |> Seq.filter (skipLines down)    
        |> Seq.map (fun line -> {line = snd line; Y = 0; TreesCounter = 0})
        |> Seq.reduce (fun previous next ->
            let position = if next.line.Length > previous.Y+right then previous.Y+right else previous.Y+right - next.line.Length
            {
                line = next.line; 
                Y = position; 
                TreesCounter = previous.TreesCounter + if next.line.[position] = '#' then 1 else 0
            })
    result.TreesCounter

let Main =
    let map = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_3_input.txt")
    
    let a = GoToboggan map 1 1
    let b = GoToboggan map 3 1
    let c = GoToboggan map 5 1
    let d = GoToboggan map 7 1
    let e = GoToboggan map 1 2

    printfn "Puzzle 1:"
    printfn "%A" b

    printfn ""
    printfn "Puzzle 2:"
    printfn "%A" a
    printfn "%A" b
    printfn "%A" c
    printfn "%A" d
    printfn "%A" e
    printfn "%A" (a * b * c * d * e )



module Day3

type Movement = { line: string; Y: int; TreesCounter: int }
    
let GoToboggan map =    
    map
    |> Seq.map (fun line -> {line = line; Y = 0; TreesCounter = 0})
    |> Seq.reduce (fun previous next -> 
        let position = if next.line.Length > previous.Y+3 then previous.Y+3 else previous.Y+3 - next.line.Length
        {
            line = next.line; 
            Y = position; 
            TreesCounter = previous.TreesCounter + if next.line.[position] = '#' then 1 else 0
        })        

let Main =
    let map = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_3_input.txt")
    printfn "%A" (GoToboggan map)

namespace moroni.aoc.y2020

module Day7 = 

    open System.IO    
    
    let readLines filename = File.ReadAllLines filename

    let contains x = String.exists ((=) x)

    let rec bagContains (bags:string list) (lines:string list) =
        if Seq.length bags = 0 
            then []
            else
                let result = 
                    lines
                    |> Seq.filter (fun l -> bags |> Seq.exists (fun b -> l.Substring(1).Contains(b)))
                    |> Seq.map (fun l -> l.Substring(0, l.IndexOf(" bags")))
                    |> List.ofSeq
                (bagContains result lines) @ result
        

    let Main =
        let lines = 
            readLines "App_Data/view-source_https___adventofcode.com_2020_day_7_input.txt"
            |> List.ofArray
        
        let bags = 
            lines
            |> bagContains ["shiny gold"]
            |> List.distinct

        printfn "Day 6 - Part 1: %A" bags.Length

        //printfn "Day 6 - Part 2: %A" 0
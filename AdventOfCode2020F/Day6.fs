namespace moroni.aoc.y2020

module Day6 = 

    open System.IO
    
    let readLines filename = File.ReadAllLines filename

    let splitSections lines =
        Array.fold (fun grouped line ->
            match line with
            | "" -> [] :: grouped
            | line -> (line :: grouped.Head) :: grouped.Tail
        ) [[]] lines    

    let Main =
        let lines = readLines "App_Data/view-source_https___adventofcode.com_2020_day_6_input.txt"
        
        let sections = splitSections lines       

        let yesAnyoneCount = 
            sections
            |> Seq.map (fun section -> section |> Seq.reduce (+))
            |> Seq.map (fun answers -> 
                answers                
                |> Seq.groupBy (fun s -> s)
                )
            |> Seq.map (fun y -> y |> Seq.length)
            |> Seq.sum

        let yesEveryoneCount = 
            sections            
            |> Seq.map (fun section -> 
                section 
                |> Seq.map (fun s-> s |> Seq.distinct)
                |> Seq.collect id
                |> Seq.countBy id
                |> Seq.sumBy (fun (c, i) -> if i = section.Length then 1 else 0)
                )
            |> Seq.sum
                                          
        printfn "Day 6 - Part 1: %A" yesAnyoneCount

        printfn "Day 6 - Part 2: %A" yesEveryoneCount

         

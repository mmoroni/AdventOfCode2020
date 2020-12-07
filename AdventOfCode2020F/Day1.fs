namespace moroni.aoc.y2020

module Day1 =

    let FindPair numbers =
        numbers
        |> Seq.map (fun n -> 
            numbers 
            |> Seq.where (fun m -> m <> n)
            |> Seq.where (fun m -> m + n = 2020)
            |> Seq.map (fun m -> (n, m, n + m, n * m))) 
        |> Seq.concat 

    let FindTrio numbers =         
        numbers
        |> Seq.map (fun f -> 
            numbers 
            |> Seq.map (fun s -> 
                numbers 
                |> Seq.where (fun t -> f + s + t = 2020)
                |> Seq.map (fun t -> (f, s, t, f * s * t))))
        |> Seq.concat
        |> Seq.concat

    let Main =
        let result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_1_input.txt")    
    
        let numbers = 
            Seq.ofArray result
            |> Seq.map (fun n -> n |> int)
            
        printfn "%A" (FindPair numbers)
        printfn "%A" (FindTrio numbers)         
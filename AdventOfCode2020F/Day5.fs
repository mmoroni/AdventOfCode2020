namespace moroni.aoc.y2020

module Day5 = 

    let ConvertToSeatId (seatCode:string) =
        let seatCode = seatCode.Replace('F', '0')
        let seatCode = seatCode.Replace('B', '1')
        let seatCode = seatCode.Replace('L', '0')
        let seatCode = seatCode.Replace('R', '1')
        let row = "0b" + seatCode.Substring(0, 7) |> int
        let col = "0b" + seatCode.Substring(7, 3) |> int
        row * 8 + col

    let FindSantaSeat (input:string[]) = 
        input    
        |> Seq.map ConvertToSeatId
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.pick (fun (p, n) -> if (p + 2 = n) then Some(p + 1) else None)

    let Main =
        let input = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_5_input.txt")    

        let santaSeatId = 
            input
            |> FindSantaSeat

        printfn "%A" (santaSeatId)
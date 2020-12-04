module Day4
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseHairColor str =
   match str with
     | ParseRegex "^#([a-f0-9]{6})$" [m] -> true
     | _ -> false

let parseEyeColor str =
   match str with
     | ParseRegex "^(amb|blu|brn|gry|grn|hzl|oth)$" [m] -> true
     | _ -> false

let parsePassport str =
   match str with
     | ParseRegex "^(\d{9})$" [m] -> true
     | _ -> false

let parseHeight str =
   match str with
     | ParseRegex "^(\d+)in$" [m] -> m |> int |> (fun m -> 59 <= m && m <= 76)
     | ParseRegex "^(\d+)cm$" [m] -> m |> int |> (fun m -> 150 <= m && m <= 193)
     | _ -> false

let Part2 raw  =    
    let single = 
        raw        
        |> Seq.map (fun n -> n + " ")
        |> Seq.map (fun n -> if n = " " then "|" else n)        
        |> Seq.reduce (+)
    single.Split "|"
    |> Seq.map (fun n -> n.Split(' '))
    |> Seq.where (fun n -> 
        n |> Seq.where (fun n -> n.Split(":").[0]  = "byr" && n.Split(":").[1] |> int >= 1920 && n.Split(":").[1] |> int <= 2002) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "iyr" && n.Split(":").[1] |> int >= 2010 && n.Split(":").[1] |> int <= 2020) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "eyr" && n.Split(":").[1] |> int >= 2020 && n.Split(":").[1] |> int <= 2030) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "hgt" && n.Split(":").[1] |> parseHeight) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "hcl" && n.Split(":").[1] |> parseHairColor) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "ecl" && n.Split(":").[1] |> parseEyeColor ) |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "pid" && n.Split(":").[1] |> parsePassport) |> Seq.length |> (fun n -> n > 0))
    |> Seq.length


let Part1 raw  =    
    let single = 
        raw        
        |> Seq.map (fun n -> n + " ")
        |> Seq.map (fun n -> if n = " " then "|" else n)        
        |> Seq.reduce (+)
    single.Split "|"
    |> Seq.map (fun n -> n.Split(' '))
    |> Seq.where (fun n -> 
        n |> Seq.where (fun n -> n.Split(":").[0]  = "byr") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "iyr") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "eyr") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "hgt") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "hcl") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "ecl") |> Seq.length |> (fun n -> n > 0) &&
        n |> Seq.where (fun n -> n.Split(":").[0]  = "pid") |> Seq.length |> (fun n -> n > 0))
    |> Seq.length


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
    let raw = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_4_input.txt")    
    
    printfn "%A" (Part1 raw)
    printfn "%A" (Part2 raw)


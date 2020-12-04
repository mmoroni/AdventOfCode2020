module Day4

open System.IO
open System.Text.RegularExpressions

type Passport = {
    byr: string option // Birth Year
    iyr: string option // Issue Year
    eyr: string option // Expiration Year
    hgt: string option // Height
    hcl: string option // Hair Color
    ecl: string option // Eye Color
    pid: string option // Passport ID
    cid: string option // Country ID
}

let readLines filename = File.ReadAllLines filename

let splitPassportSections lines =
    Array.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail
    ) [[]] lines

let passportKeyValueRegex = Regex(@"(\w+):([^ ]+)", RegexOptions.Compiled)

let parsePassportMapOfLines lines =
    lines
    |> Seq.collect passportKeyValueRegex.Matches
    |> Seq.map (fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
    |> Map.ofSeq

let parsePassportOfMap map =
    let tryFind key = Map.tryFind key map
    {
        byr = tryFind "byr"
        iyr = tryFind "iyr"
        eyr = tryFind "eyr"
        hgt = tryFind "hgt"
        hcl = tryFind "hcl"
        ecl = tryFind "ecl"
        pid = tryFind "pid"
        cid = tryFind "cid"
    }

let validatePassport passport =
    passport.byr <> None
    && passport.iyr <> None
    && passport.eyr <> None
    && passport.hgt <> None
    && passport.hcl <> None
    && passport.ecl <> None
    && passport.pid <> None
    //&& passport.cid <> None

let validateDateRange date min max =
    match date with
        | Some(d) -> d |> int >= min && d |> int <= max
        | None -> false
    
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let validateHairColor str =
    match str with
    | Some(s) ->
        match s with           
        | ParseRegex "^#([a-f0-9]{6})$" [m] -> true
        | _ -> false
    | None -> false 

let validateEyeColor str =
    match str with
    | Some(s) ->
        match s with        
            | ParseRegex "^(amb|blu|brn|gry|grn|hzl|oth)$" [m] -> true
            | _ -> false
    | None -> false 

let validatePassportNumber str =
   match str with
   | Some(s) ->
        match s with
        | ParseRegex "^(\d{9})$" [m] -> true
        | _ -> false
   | None -> false 

let validateHeight str =
   match str with
   | Some(s) ->
        match s with
        | ParseRegex "^(\d+)in$" [m] -> m |> int |> (fun m -> 59 <= m && m <= 76)
        | ParseRegex "^(\d+)cm$" [m] -> m |> int |> (fun m -> 150 <= m && m <= 193)
        | _ -> false
   | None -> false   

let advancedPassport passport =
    validateDateRange passport.byr 1920 2002
    && validateDateRange passport.iyr 2010 2020
    && validateDateRange passport.eyr 2020 2030
    && validateHeight passport.hgt
    && validateHairColor passport.hcl 
    && validateEyeColor passport.ecl 
    && validatePassportNumber passport.pid     

let Main =
    let lines = readLines "App_Data/view-source_https___adventofcode.com_2020_day_4_input.txt"
    printfn "Read %i lines" lines.Length    

    let passportSections = splitPassportSections lines |> Array.ofList
    printfn "Found %i passports" passportSections.Length

    let passports =
        passportSections
        |> Array.map parsePassportMapOfLines
        |> Array.map parsePassportOfMap

    let validPassports =
        passports
        |> Array.filter validatePassport

    printfn "Day 4 - Part 1: %i" validPassports.Length    

    let validPassports =
        passports
        |> Array.filter advancedPassport

    printfn "Day 4 - Part 2: %i" validPassports.Length    

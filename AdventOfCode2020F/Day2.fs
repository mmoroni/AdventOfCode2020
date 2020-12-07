namespace moroni.aoc.y2020

module Day2 =

    type PasswordPolicy = { Number1: int; Number2: int; Letter: char; Password: string }

    let CreatePP (input:string) : PasswordPolicy = 
        let segments = input.Split('-', ' ', ':');
        {
            Number1 = segments.[0] |> int
            Number2 = segments.[1] |> int
            Letter = segments.[2].[0]
            Password = segments.[4]
        }        

    let CountValidPasswords (passwords:seq<PasswordPolicy>) = 
        passwords
        |> Seq.where (fun p -> 
            p.Password 
            |> Seq.where (fun c-> c = p.Letter) 
            |> Seq.length 
            |> function n -> n >= p.Number1 && n <= p.Number2)
        |> Seq.length

    let ValidateNewPolicy p = 
        let v1 = p.Password.Length >= p.Number1 && p.Password.[p.Number1-1] = p.Letter;
        let v2 = p.Password.Length >= p.Number2 && p.Password.[p.Number2-1] = p.Letter;
        v1 && not v2 || v2 && not v1

    let CountValidPasswordsNewPolicy passwords = 
        passwords
        |> Seq.where ValidateNewPolicy
        |> Seq.length

    let Main = 
        let result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_2_input.txt")    

        let passwords = 
            Seq.ofArray result
            |> Seq.map CreatePP

        printfn "%A" (CountValidPasswords passwords)      

        printfn "%A" (CountValidPasswordsNewPolicy passwords)
module Day2

type PasswordPolicy = { Minimum: int; Maximum: int; Letter: char; Password: string }

let CreatePP (input:string) : PasswordPolicy = 
    let segments = input.Split('-', ' ', ':');
    {
        Minimum = segments.[0] |> int
        Maximum = System.Int32.Parse segments.[1]
        Letter = segments.[2].[0]
        Password = segments.[4]
    }        

let CountValidPasswords (passwords:seq<PasswordPolicy>) = 
    passwords
    |> Seq.where (fun p -> 
        p.Password 
        |> Seq.where (fun c-> c = p.Letter) 
        |> Seq.length 
        |> function n -> n >= p.Minimum && n <= p.Maximum)
    |> Seq.length

let Main = 
    let result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_2_input.txt")    

    let passwords = 
        Seq.ofArray result
        |> Seq.map CreatePP

    printfn "%A" (CountValidPasswords passwords)               
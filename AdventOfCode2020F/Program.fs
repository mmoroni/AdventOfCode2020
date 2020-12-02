open System

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

[<EntryPoint>]
let main argv =

    let result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_2_input.txt")    

    let passwords = 
        Seq.ofArray result
        |> Seq.map CreatePP

    printfn "%A" (CountValidPasswords passwords)
    
    let result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_1_input.txt")    
    
    let numbers = 
        Seq.ofArray result
        |> Seq.map (fun n -> n |> int)
    
    printfn "%A" (FindPair numbers)
    printfn "%A" (FindTrio numbers)    

    0
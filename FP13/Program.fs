open FunctionalTrie
open System

// running the demo - runTrie is an recursive function that calls itself with the new TreeMap obtained after you enter some requests
[<EntryPoint>]
let main argv = 
    // TreeMap is an alias for Map<char, Tree> - when you first start the program you need to enter an empty map: Map([])
    let rec runTrie treeMap =
        let rec enteredWords () = 
            Console.ReadLine() 
            |> fun x -> x.Split(',') 
            |> Array.map (fun word -> word.Trim())
            |> Array.filter (fun word -> word.Length > 0)
            |> fun arr -> if arr.Length = 0 then Console.WriteLine("An empty string is not a word. Try again!\n"); enteredWords() else arr
    
        Console.WriteLine("Please state the next operation you want to perform: enter \"i\" for insert, \"e\" for interogation or \"x\" for exit!\n")
    
        match Console.ReadLine() with
        | x when x.ToLower() = "i" -> 
            Console.WriteLine("Please enter the word you want to add (or multiple words separated by comma): \n")
            runTrie (enteredWords() |> Array.fold (fun treeMap' word -> insertWord treeMap' word) treeMap)        
        | x when x.ToLower() = "e" ->
            Console.WriteLine("Please enter the word you want to check (or multiple words separated by comma): \n")
            enteredWords() |> Array.iter (fun word -> Console.WriteLine("Word \"{0}\" is {1}\n", word, existsWord treeMap word |> fun b -> if b then "present" else "absent"))
            runTrie treeMap
        | x when x.ToLower() = "x" -> ()
        | _ -> 
            Console.WriteLine("Please try again!")
            runTrie treeMap
    
    runTrie (Map([]))
    0 

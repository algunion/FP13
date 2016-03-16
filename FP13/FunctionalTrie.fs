// Bellow is the almost pure functional trie (or at least some sort of "functional simulation") we tried to implement at "fp meetup v13" in Cluj-Napoca
// We needed to print some text to console therefore "almost pure functional". Anyway, the algorithm for the trie itself is side-effects free.

module FunctionalTrie
open System


// Mutual recursive types implementing the trie
type TreeMap = Map<char, Tree>
and Tree =
    | Node of char * TreeMap
    | WordEnd of char * TreeMap

let wordToChar (word : string) = word.ToCharArray () |> List.ofArray

// checks if a word was added (please note that if you add the word "haskell" and you check wheter "has" exists the function will return false
let existsWord (treeMap: TreeMap) (word: string) =
    let charWord = word |> wordToChar
    let rec mapExists (treeMap: TreeMap) xs =
        match xs with
        | [] -> false
        | x::_ when treeMap.ContainsKey x -> treeExists treeMap.[x] xs
        | x::_ -> false 
    and treeExists (tree: Tree) xs =
        match tree, xs with
        | Node(c, treeMap), x::xs' -> mapExists treeMap xs'
        | WordEnd(c, treeMap), x::[] -> true
        | WordEnd(c, treeMap), x::xs' -> mapExists treeMap xs'
        | _, _ -> false
    mapExists treeMap charWord

let insertWord (treeMap: TreeMap) (word: string) =
    let charWord = word |> wordToChar
    let exists = existsWord treeMap word
    let rec insertMap (treeMap: TreeMap) xs =
        match xs with
        | [] -> treeMap
        | x::_ when treeMap.ContainsKey x -> treeMap.Add(x, insertTree treeMap.[x] xs)
        | x::_ -> treeMap.Add(x, createTree xs)
    and insertTree tree xs =
        match tree, xs with
        | _, [] -> failwith "insertMap function should prevent this pattern."
        | Node(c, treeMap), [x] -> WordEnd(c, treeMap)
        | Node(c, treeMap), x::xs -> Node(c, insertMap treeMap xs)
        | WordEnd(c, treeMap), x::xs -> WordEnd(c, insertMap treeMap xs) 
    and createTree xs =
        match xs with
        | [] -> failwith "insertMap function should prevent this pattern."
        | [x] -> WordEnd(x, Map([]))
        | x::y::xs -> Node(x, Map([y, createTree (y::xs)]))
    match exists with
    | true -> treeMap
    | false -> insertMap treeMap charWord

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
    
    
    

        
    


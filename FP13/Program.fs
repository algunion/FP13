﻿open FunctionalTrie

// running the demo - runTrie is an recursive function that calls itself with the new TreeMap obtained after you enter some requests
[<EntryPoint>]
let main argv = 
    runTrie (Map([]))
    0 

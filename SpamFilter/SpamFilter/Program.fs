module Program

open System
open System.IO

type Error = 
    |ArgumentNotValidPath
    |ArgumentOutOfRangeException
    |ArgumentNotValidInteger
    
type Execution = 
    |Started
    |Finished

type TMessage<'Success, 'Err> = 
    |Error of 'Err
    |Ok of 'Success
    |Execution of 'Success

let getPhraseFilePath = 
    let baseDirectory = __SOURCE_DIRECTORY__
    let baseDirectory' = Directory.GetParent(baseDirectory)
    Path.Combine(baseDirectory'.FullName, "SpamFilter\phrases_test_case\phrases.txt")

let wordSplit (text:string) = 
  text.Split([|' ';'\n';'\r';'\t';'!';',';'.';'?';';';':'; '/'
  ;'\\';'-';'+'; '*'; '#';'(';')';'^';'"';'\'';'`'; '@';'~';'|'|]
  ,StringSplitOptions.RemoveEmptyEntries)
  |> Array.toList 
  
let readPhrases (path: string)= 
    File.ReadAllLines path
    |> Array.toList 
    |> List.map (fun x -> x.ToLower()) 
    |> List.map (fun x -> wordSplit x)

let textToList (path:string) = 
    let text = File.ReadAllText path
    wordSplit (text.ToLower())
 
let printOutput x (filterPlank:int) (test:int) = 
    match x with
    | Ok(x) ->
        match x with
        |node when node >= filterPlank ->
            printfn "Test %d: Spam filtration result %d. Message marked as spam" (test) (x)
        |_ -> 
            printfn "Test %d: Spam filration result %d. Message added to inbox" (test) (x) 
    |Error(ArgumentNotValidPath) ->
        printfn "\nERROR: Not all arguments provided are valid paths.\n" 
    |Error(ArgumentNotValidInteger) -> 
        printfn "\nERROR: The first arguement provided is not valid 32-bit integer\n"
    |Error(ArgumentOutOfRangeException) -> 
        printfn "\nERROR: Provided filtering criteria must be in range from 1 to 50\n"
        
let executionMessages x = 
    match x with
    |Execution(Started) ->
        printfn "\n...Email Spam Filtration Program Invoked...\n"
    |Execution(Finished) ->
        printfn "\n...Finished All Filtration Processes...\n"
    |_ -> printfn "\nWrong type provided to the 'executionMessages()' function\n"

let rec checkPathsValidity (args: string list) = 
    match args with
    |[] -> true
    | x::xs when File.Exists x ->
        checkPathsValidity xs
    | _ -> false  

let rec matchTails (tail1 : string list) (tail2 : string list) = 
    match tail1, tail2 with
        | h1::t1 , h2::t2 -> 
            if (h1=h2) then 
                matchTails t1 t2
            else
                false
        | [], _ -> false
        | _, []  -> true

let rec phraseProcessor (textH: string) (textT: string list) (phrases: string list list) (n:int) = 
    match phrases with 
    |[] -> n
    | h :: t ->
        match h with
        |x when x.Head = textH && (matchTails (textT) (x.Tail)) ->
            phraseProcessor (textH) (textT) (t) (n+1)
        | _ -> 
            phraseProcessor (textH) (textT) (t) (n)
            

let rec wordChanger (phrases : string list list) (text:string list) (n:int)= 
    match text with
    | [] -> n
    | h :: t ->
        wordChanger phrases t (phraseProcessor (h) (t) (phrases) (n))

let rec startFiltration (filterCriteria:int) (phrasePath: string list list) (textsPath: string list) (testCount: int)= 
    match textsPath with 
    | [] -> executionMessages (Execution(Finished))
    | h :: t -> 
        let result = wordChanger (phrasePath) (textToList (h)) (0)
        printOutput (Ok(result)) (filterCriteria) (testCount) 
        startFiltration (filterCriteria) (phrasePath) (t) (testCount+1)

let checkAndStart (args: string list) = 
    match (Int32.TryParse(args.Head)) with
    | (valid, n) when valid && (n >= 1) && (n <= 50) ->
        match (checkPathsValidity (args.Tail)) with 
        |true -> 
            executionMessages (Execution(Started))
            startFiltration (n) (getPhraseFilePath |> readPhrases) (args.Tail) (1)
        |false -> 
            printOutput(Error(ArgumentNotValidPath)) 0 0
    | (valid, n) when valid && ((n<1) ||(n>50)) ->
        printOutput(Error(ArgumentOutOfRangeException)) 0 0
    | _ -> 
        printOutput(Error(ArgumentNotValidInteger)) 0 0
       

[<EntryPoint>]
let main argv = 
    argv |> Array.toList |> checkAndStart
    0
open System

let readInput (fileName: string): string list =
    System.IO.File.ReadLines(fileName) |> Seq.toList

let sortedCharacterCounts(list: string list, index: int): (char * int) list =
    list |> 
    List.map(fun y -> y.Chars(index)) |> 
    List.groupBy(fun c -> c) |>
    List.map(fun (c, l) -> (c, l.Length)) |>
    List.sortBy(fun (c, l) -> l)

let mostFrequentCharacterByIndex(list: string list, index: int): char =
    sortedCharacterCounts(list, index) |> List.last |> fun (c, i) -> c

let leastFrequentCharacterByIndex(list: string list, index: int): char =
    sortedCharacterCounts(list, index) |> List.head |> fun (c, i) -> c

let calculatePart1 (inputList: string list): int = 
    let mostFrequentCharacterByIndex = 
        seq { 0..(inputList.Head.Length-1) } |> 
        Seq.toList |> 
        List.map (fun x -> mostFrequentCharacterByIndex(inputList, x))

    let binaryGamma = mostFrequentCharacterByIndex |> Array.ofList |> String
    let binaryGammaInverse = binaryGamma.ToCharArray() |> Array.map(fun c -> if c.Equals('0') then '1' else '0') |> String
    
    let gamma = Convert.ToInt32(binaryGamma, 2);
    let epsilon = Convert.ToInt32(binaryGammaInverse, 2)

    gamma * epsilon

let filterByMostFrequentCharacter(list: string list, index: int): string list = 
    list |> List.filter(fun s -> s.Chars(index).Equals(mostFrequentCharacterByIndex(list, index)))

let filterByLeastFrequentCharacter(list: string list, index: int): string list = 
    list |> List.filter(fun s -> s.Chars(index).Equals(leastFrequentCharacterByIndex(list, index)))

let calculatePart2 (inputList: string list): int =
    let mf x y = filterByMostFrequentCharacter (y, x)
    let lf x y = filterByLeastFrequentCharacter (y, x)

    // r/badcode
    let filteredByMostFrequent = inputList |> (mf 0) |> (mf 1) |> (mf 2) |> (mf 3) |> (mf 4) |> (mf 5) |> (mf 6) |> (mf 7) |> (mf 8) |> (mf 9) |> (mf 10) |> (mf 11)
    let filteredByLeastFrequent = inputList |> (lf 0) |> (lf 1) |> (lf 2) |> (lf 3) |> (lf 4) |> (lf 5) |> (lf 6) |> (lf 7) |> (lf 8) |> (lf 9) |> (lf 10) |> (lf 11)

    let oxygenGeneratorRatingBinary = filteredByMostFrequent |> List.head;
    let co2ScrubberRatingBinary = filteredByLeastFrequent |> List.head;

    let oxygenGeneratorRating = Convert.ToInt32(oxygenGeneratorRatingBinary, 2);
    let co2ScrubberRating = Convert.ToInt32(co2ScrubberRatingBinary, 2)
    
    oxygenGeneratorRating * co2ScrubberRating

[<EntryPoint>]
let main argv =
    let inputList = readInput "input.txt"
    printfn "%d" (calculatePart1 inputList)
    printfn "%d" (calculatePart2 inputList)
    0
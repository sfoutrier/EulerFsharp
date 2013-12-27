open System.Diagnostics

// trick for debugging display
System.Linq.Enumerable.Count([]) |> ignore

type value =
    | C2 = 1
    | C3 = 2
    | C4 = 3
    | C5 = 4
    | C6 = 5
    | C7 = 6
    | C8 = 7
    | C9 = 8
    | C10 = 9
    | Jack = 10
    | Queen = 11
    | King = 12
    | Ace = 13

type suit =
    | H
    | C
    | S
    | D

type Card(toParse : string) =
    member this.value =
        match toParse.[0] with
            | '2' -> value.C2
            | '3' -> value.C3
            | '4' -> value.C4
            | '5' -> value.C5
            | '6' -> value.C6
            | '7' -> value.C7
            | '8' -> value.C8
            | '9' -> value.C9
            | 'T' -> value.C10
            | 'J' -> value.Jack
            | 'Q' -> value.Queen
            | 'K' -> value.King
            | 'A' -> value.Ace
            | _ -> failwith "Unreadable card value"
    member this.suit =
        match toParse.[1] with
            | 'H' -> suit.H
            | 'C' -> suit.C
            | 'S' -> suit.S
            | 'D' -> suit.D
            | _ -> failwith "Unreadable card suit"
    override this.ToString() =
        sprintf "%A of %A" this.value this.suit

let compareCards c1 c2 =
    Seq.zip c1 c2
        |> Seq.filter (function a, b when a = b -> false | _ -> true)
        |> Seq.head |> function a, b -> a > b

[<AbstractClass>]
type Hand(sortedCardsValue) =
    member this.cards = sortedCardsValue |> Seq.toList
    member this.compareCard(other : Hand) =
        Seq.zip this.cards other.cards
            |> Seq.filter (function a, b when a = b -> false | _ -> true)
            |> Seq.head |> function a, b -> a > b
    abstract member winAgainst : Hand -> bool

type HighCard(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "%A" sortedCards

type OnePair(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard -> true
        | :? OnePair -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "Pair<%A> %A" (List.head sortedCards) (List.tail sortedCards)

type TwoPair(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair -> true
        | :? TwoPair -> compareCards this.cards otherHand.cards
        | _ -> false
    override this.ToString() =
        sprintf "2 Pair<%A-%A> %A" (List.head sortedCards) (List.head (List.tail sortedCards)) (List.tail (List.tail sortedCards))

type ThreeOfAKind(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair | :? TwoPair -> true
        | :? ThreeOfAKind -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "Three<%A> %A" (List.head sortedCards) (List.tail sortedCards)

type Straight(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair | :? TwoPair | :? ThreeOfAKind -> true
        | :? Straight -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "Straight %A" sortedCards

type Flush(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair | :? TwoPair | :? ThreeOfAKind | :? Straight -> true
        | :? Flush -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "Flush %A" sortedCards
        
type FullHouse(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair | :? TwoPair | :? ThreeOfAKind | :? Straight | :? Flush -> true
        | :? FullHouse -> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "FullHouse<%A-%A>" (List.head sortedCards) (List.head (List.tail sortedCards))

type FourOfAKind(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(otherHand) =
        match otherHand with
        | :? HighCard | :? OnePair | :? TwoPair | :? ThreeOfAKind | :? Straight | :? Flush | :? FullHouse -> true
        | :? FourOfAKind-> this.compareCard otherHand
        | _ -> false
    override this.ToString() =
        sprintf "Four<%A> %A" (List.head sortedCards) (List.tail sortedCards)

type StraightFlush(sortedCards) =
    inherit Hand(sortedCards)
    override this.winAgainst(other) =
        match other with
        | :? HighCard | :? OnePair | :? TwoPair | :? ThreeOfAKind | :? Straight | :? Flush | :? FullHouse | :? FourOfAKind -> true
        | :? StraightFlush -> this.compareCard other
        | _ -> false
    override this.ToString() =
        sprintf "StraightFlush %A" sortedCards

// I don't care about royal flush, it's already handled by the Straight Flush

let tryFindSame n sortedCards =
    Seq.countBy (fun v -> v) sortedCards
    |> Seq.tryFind (fun (v,l) -> l=n)
    |> Option.map
        (fun (v,l) ->
            (Seq.append
                (Seq.singleton v)
                (Seq.filter (fun v1 -> v1 <> v) sortedCards))
            |> Seq.toList)

let tryParseOnePair (sortedCards:value list) =
    tryFindSame 2 sortedCards
    |> Option.map (fun sorted -> OnePair(sorted))

let tryParseTwoPair cards =
    (match
        (Seq.groupBy (fun c -> c) cards
            |> Seq.map (fun (v, c) -> (v, Seq.length c))
            |> Seq.filter (fun (v,l) -> l=2)
            |> Seq.map fst
            |> Seq.toList) with
        | pairs when List.length pairs = 2 
            -> Some(TwoPair((Seq.append
                                pairs
                                (List.filter 
                                    (fun v -> List.exists (fun v2 -> v = v2) pairs)
                                    cards)) |> Seq.toList))
        | _ -> None)
            
let tryParseThreeOfAKind cards =
    tryFindSame 3 cards
    |> Option.map (fun v -> ThreeOfAKind(v))

let tryParseFourOfAKind cards =
    tryFindSame 4 cards
    |>  Option.map (fun v -> FourOfAKind(v))
    
let isStraight sortedCards =
    match sortedCards with 
    | head::tail ->
        Seq.fold
            (fun p v -> match p with 
                        | None -> None 
                        | Some(pv) -> 
                            match (int(pv) - int(v)) with 1 -> Some(v) | _ -> None)
            (Some(head))
            tail
    | [] -> failwith "No cards to match"
    |> Option.isSome

let isFlush (cards: Card list) =
    match cards with
    | head::tail ->
        Seq.forall (fun (i:Card) -> head.suit = i.suit) tail
    | [] -> failwith "No cards to match"

let tryParseFullHouse sortedCards =
    let grouped = Seq.countBy (fun v -> v) sortedCards
    match (Seq.tryFind (fun (v,l) -> l=3) grouped, Seq.tryFind (fun (v,l) -> l=2) grouped) with
    | Some(a), Some(b) -> Some(FullHouse([fst a;fst b]))
    | _ -> None

let parseHand cards =
    let sortedValues = Seq.map (fun (c:Card) -> c.value) cards |> Seq.toList |> List.sort |> List.rev
    let straight = isStraight sortedValues
    let flush = isFlush cards
    match straight && flush with
    | true -> StraightFlush(sortedValues) :> Hand
    | false ->
        match tryParseFourOfAKind sortedValues with
        | Some(h) -> h :> Hand
        | None ->
            match tryParseFullHouse sortedValues with
            | Some(h) -> h :> Hand
            | None ->
                match flush with
                | true -> Flush(sortedValues) :> Hand
                | false ->
                    match straight with
                    | true -> Straight(sortedValues) :> Hand
                    | false ->
                        match tryParseThreeOfAKind sortedValues with
                        | Some(h) -> h :> Hand
                        | None ->
                            match tryParseTwoPair sortedValues with
                            | Some(h) -> h :> Hand
                            | None ->
                                match tryParseOnePair sortedValues with
                                | Some(h) -> h :> Hand
                                | None -> HighCard(sortedValues) :> Hand
            
[<EntryPoint>]
let main args =
    let lines = System.IO.File.ReadLines("../../poker.txt")

    let res = Seq.map
                    (fun (l:string) -> 
                        l.Split(' ')
                        |> Seq.fold 
                            (fun (a,b) i -> match List.length a with 
                                                | 5 -> (a, Card(i)::b) 
                                                | _ -> (Card(i)::a, b)) ([], []) )
                    lines
                |> Seq.map (fun (a,b) -> 
                    let handA = parseHand a
                    let handB = parseHand b
                    let aWin = handA.winAgainst(handB)
//                    printfn "%A" a
//                    printfn "%A" b
//                    printfn "=> %A vs %A => %A" handA handB aWin
                    aWin)
                |> Seq.fold (fun c w -> match w with true -> c+1 | false -> c) 0

    printfn "%d" res

    let _ = System.Console.ReadKey true
    0
namespace SRTV

module Substitution =
    open System
    open System.Text.RegularExpressions

    open Humanizer

    let punctuation = @"[.;,!?\(\)]"
    let whitespaceBoundaryStart = $@"(?<startBoundary>\s|^|{punctuation})"
    let whitespaceBoundaryEnd = $@"(?<endBoundary>\s|$|{punctuation})"

    module Punctuation =
        let simpleReplacement =
            [
                ("_", "underscore");
                ("%", "percent");
                ("#", "hashtag");
                ("°", "degree");
                ("@", "at");
                ("&", "and")
            ]

        let removal = ['-'; '('; ')']

    module Emojis =
        open FSharp.Data

        type Emoji = CsvProvider<"./assets/emojis.csv", HasHeaders=true>
        type Trie<'a> = 
            Node of key:string * value:'a option * children:Trie<'a> list
                static member hasKey key (Node (k, _, _)) = key = k
                static member key (Node (k, _, _)) = k
                static member tryFindChild<'a> key (Node (_, _, children)) =
                    List.tryFind (Trie.hasKey<'a> key) children
                static member emptyTrie = Node ("", None, [])

        let rec tryFindNext text (Node (k, v, _) as node) =
            match text with
            | ""    -> Option.map (fun x -> (k.Length, x)) v
            | t     ->
                let endIndex = if Char.IsSurrogatePair(text, 0) then 1 else 0
                let key = t.[ .. endIndex ]
                Trie.tryFindChild key node
                |> Option.bind (tryFindNext text.[endIndex + 1 .. ])
                |> Option.map (fun (length, v) -> (length + k.Length, v))
                |> Option.orElse (Option.map (fun x -> (k.Length, x)) v)

        let rec add codepoints value (Node (k1, v1, children) as node) =
            match codepoints with
            | ""    ->  Node (k1, Some value, children)
            | cs    ->
                let endIndex = if Char.IsSurrogatePair(codepoints, 0) then 1 else 0
                let key = cs.[ .. endIndex ]
                let child =
                    Trie.tryFindChild key node 
                    |> Option.defaultValue (Node (key, None, []))
                    |> add cs.[ endIndex + 1 .. ] value
                
                Node (k1, v1, List.distinctBy Trie.key<string> (child::children))

        let private emojiTrie = 
            Emoji.GetSample().Rows 
            |> Seq.fold (fun trie emoji -> add emoji.Emoji emoji.Name trie) Trie<string>.emptyTrie

        let parseForEmoji text = tryFindNext text emojiTrie

    module Numbers =
        let toWords : int64 -> string = Humanizer.NumberToWordsExtension.ToWords
        let toOrdinalWords =  Humanizer.NumberToWordsExtension.ToOrdinalWords

        let private bindRegex pattern = whitespaceBoundaryStart + pattern + whitespaceBoundaryEnd

        let wholeNumberPattern = @"(?<wholeNumber>\d{1,3}(,\d{3})+|\d+)"
        let decimalPattern = $@"(?<integral>{wholeNumberPattern})?\.(?<fractional>\d+)"

        let timePattern = @"(?<hour>[0-1]?\d|2[0-3]):(?<minute>[0-5]\d)(?<between>\s*)(?<meridiem>(?:(?:[aA]|[Pp])[Mm])?)"
        
        //TODO: change function to change decimals without a leading number (e.g .75)
        let processDecimals text =
            let evaluator (m:Match) =
                let integral = int64 m.Groups.[2].Value |> toWords
                let fractional = Seq.toList m.Groups.[3].Value |> List.map (string >> int64 >> toWords)
                $"""%s{m.Groups.[1].Value}%s{integral} point %s{String.concat " " fractional}"""
            Regex.Replace(text, @"(^|\s)(\d+)\.(\d+)", MatchEvaluator(evaluator))

        let processWholeNumbers text =
            let evaluator (m:Match) =
                let number = m.Groups.["wholeNumber"].Value.Replace(",", "") |> int64 |> toWords
                m.Groups.["startBoundary"].Value + number + m.Groups.["endBoundary"].Value
            Regex.Replace (text, bindRegex wholeNumberPattern, MatchEvaluator(evaluator))

        //let processWholeNumbers text =
        //    let evaluator (m:Match) = toWords <| int64 m.Value
        //    Regex.Replace (text, @"\d+", MatchEvaluator(evaluator))

        let processRanges text =
            let evaluator (m:Match) =
                let left = m.Groups.["left"].Value
                let right = m.Groups.["right"].Value
                let hasPeriod = String.exists (fun c -> c = '.')
                let numbersToWords text = 
                    if hasPeriod text then processDecimals text else processWholeNumbers text
                $"{numbersToWords left} to {numbersToWords right}"
            Regex.Replace (text, @"(?<left>\d+|\d*(?:\.\d+))-(?<right>\d+|\d*(?:\.\d+))", MatchEvaluator(evaluator))
        
        let processOrdinals text =
            let evaluator (m:Match) =
                sprintf "%s%s%s%s"
                    <| m.Groups.["start"].Value
                    <| if m.Groups.["startNumber"].Success then (int64 >> toWords) m.Groups.["startNumber"].Value else ""
                    <| (int >> toOrdinalWords) m.Groups.["endNumber"].Value
                    <| m.Groups.["end"].Value
            Regex.Replace(text, @"(?<start>^|\s)(?<startNumber>\d+?)?(?:(?<endNumber>\d?1)st|(?<endNumber>\d?2)nd|(?<endNumber>\d?3)rd|(?<endNumber>\d?[04-9])th)(?<end>\s|$)", MatchEvaluator(evaluator))

        let processAmericanPhoneNumbers text =
            let evaluator (m:Match) =
                let number = 
                    m.Groups.["number"].Value.Replace("-", "").Replace(" ", "")
                    |> Seq.map (string >> Convert.ToInt64 >> toWords)
                    |> Seq.indexed
                    |> Seq.groupBy (fun (index, _) -> if index < 3 then 0 else if index < 6 then 1 else 2)
                    |> Seq.map (fun (_, group) -> Seq.map snd group |> String.concat " ")
                    |> String.concat ". "
                m.Groups.["start"].Value + number + m.Groups.["end"].Value
            Regex.Replace(text, @"(?<start>^|\s)(?<number>\d{3}(?:[ \-])\d{3}(?:[ \-])\d{4})(?<end>\s|$)", MatchEvaluator(evaluator))

        let processPhoneNumbers = processAmericanPhoneNumbers
        
        let processTimes text =
            let evaluator (m:Match) =
                let hour = int64 m.Groups.["hour"].Value
                let minute = int64 m.Groups.["minute"].Value
                let minuteWords =
                    match (hour, minute) with
                    | (h, 0L) when h > 12L -> "hundred hours"
                    | (_, 0L) -> ""
                    | (_, m) when m < 10L -> $"oh {toWords m}"
                    | (_, m) -> toWords m  
                let meridiem = m.Groups.["meridiem"].Value
                let time =
                    let meridiem = 
                        meridiem.ToLower() 
                        |> Seq.map string 
                        |> String.concat " " 
                        |> fun x -> if x = "" then "" else $" {x}"
                    $"{toWords hour} {minuteWords}{meridiem}"
                m.Groups.["startBoundary"].Value + time + m.Groups.["endBoundary"].Value

            Regex.Replace(text, bindRegex timePattern, MatchEvaluator(evaluator))
                
    let rec private processEmojis' acc =
        function
        | ""    -> List.rev acc |> String.concat ""
        | text  ->
            let (length, value) = Option.defaultValue (1, text.[ .. 0]) (Emojis.parseForEmoji text)
            processEmojis' (value :: acc) text.[ length .. ]

    let private processEmojis = processEmojis' []
    let private processNumbers = 
        Numbers.processPhoneNumbers >>
        Numbers.processRanges >>
        Numbers.processTimes >>
        Numbers.processDecimals >> 
        Numbers.processOrdinals >>
        Numbers.processWholeNumbers
        
    let private simpleSubstitution = 
        Punctuation.simpleReplacement
        |> Seq.foldBack (fun (key, replacement) state -> Regex.Replace(state, key, $" {replacement} "))

    let private simpleRemoval = String.map (fun c -> if List.contains c Punctuation.removal then ' ' else c)

    //Ensure that simple substitution and simple removal are the last two functions called when processing text
    //Not doing so may lead to some unexpected incorrect processing, especially with ranges that require the "-" character
    let processSpeakText = processEmojis >> processNumbers >> simpleSubstitution >> simpleRemoval


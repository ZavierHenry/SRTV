namespace SRTV

module Substitution =
    open System
    open System.Text.RegularExpressions

    open Humanizer

    let punctuation = @"[.;,!?\(\)\\%]"
    let whitespaceBoundaryStart = $@"(?<=\s|\n|^|{punctuation})"
    let whitespaceBoundaryEnd = $@"(?=\s|$|{punctuation})"

    let private normalize (text:string) = Regex.Replace(text.Trim(), "\s{2,}", " ")

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

        let removal = ['-'; '('; ')' ; '“' ; '”']

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

        let wholeNumberPattern = @"(?<integral>\d{1,3}(,\d{3})+|\d+)"
        let decimalPattern = $@"{wholeNumberPattern}?\.(?<fractional>\d+)"
        let unsignedNumberPattern = $@"({decimalPattern}|{wholeNumberPattern})"
        let numberPattern = $@"(?<sign>\+|-)?{unsignedNumberPattern}"

        let private meridiem = @"(?<meridiem>(?:(?:[aA]|[Pp])[Mm]))"
        let private timeShortPattern = $@"(?<hour>[1-9]|1[0-2])(?<between>\s*){meridiem}"
        let timePattern = $@"(?<hour>[0-1]?\d|2[0-3]):(?<minute>[0-5]\d)(?<between>\s*){meridiem}?"
        let timeRegex = $@"(?:{timePattern}|{timeShortPattern})"

        let abbreviations = seq {
            ( bindRegex @$"(?<=(?<sign>\+)?{unsignedNumberPattern}\s+)cm", "centimeters")
            ( bindRegex @$"(?<=(?<sign>\+)?{unsignedNumberPattern}\s+)mph", "miles per hour")
            ( bindRegex @$"(?<={timeRegex}\s+)ET", "eastern time")
            ( bindRegex @$"(?<={timeRegex}\s+)CT", "central time")
            ( bindRegex @$"(?<={timeRegex}\s+)MT", "mountain time")
            ( bindRegex @$"(?<={timeRegex}\s+)PT", "pacific time")
        }

        let processAbbreviations (text:string) = 
            abbreviations
            |> Seq.fold (fun state (pattern, replacement) -> Regex.Replace(state, pattern, replacement)) text

        let replaceNumbers text =
            let evaluator (m:Match) =
                let sign = match m.Groups.["sign"].Value with | "+" -> "plus" | "-" -> "minus" | _ -> ""
                let integral = 
                    let integral = m.Groups.["integral"]
                    if integral.Success then integral.Value.Replace(",", "") |> int64 |> toWords else ""
                let fractional = 
                    match m.Groups.["fractional"] with
                    | fractional when fractional.Success ->
                        Seq.toList fractional.Value
                        |> List.map (string >> int64 >> toWords)
                        |> String.concat " "
                        |> sprintf "point %s"
                    | _ -> ""
                $"{sign} {integral} {fractional}"
            Regex.Replace(text, bindRegex numberPattern, MatchEvaluator(evaluator))

        let processRanges text =
            let evaluator (m:Match) =
                let left = m.Groups.["left"].Value
                let right = m.Groups.["right"].Value
                let numbersToWords text = 
                    replaceNumbers text
                $"{numbersToWords left} to {numbersToWords right}"
            Regex.Replace (text, @"(?<left>\d+|\d*(?:\.\d+))-(?<right>\d+|\d*(?:\.\d+))", MatchEvaluator(evaluator))
        
        let processOrdinals text =
            let evaluator (m:Match) =
                let number = m.Groups.["integral"].Value.Replace(",", "") |> int
                sprintf "%s%s"
                    <| if number >= 1000 && number <= 2000 then "one " else ""
                    <| toOrdinalWords number
            Regex.Replace(text, bindRegex $@"{wholeNumberPattern}(?:(?<=1)st|(?<=2)nd|(?<=3)rd|(?<=[04-9])th)", MatchEvaluator(evaluator))

        let processAmericanPhoneNumbers text =

            let evaluator (m:Match) =
                let number = 
                    Regex.Replace(m.Groups.["number"].Value, @"[\- .]", "")
                    |> Seq.map (string >> Convert.ToInt64 >> toWords)
                    |> Seq.indexed
                    |> Seq.groupBy (fun (index, _) -> if index < 3 then 0 else if index < 6 then 1 else 2)
                    |> Seq.map (fun (_, group) -> Seq.map snd group |> String.concat " ")
                    |> String.concat ". "
                m.Groups.["start"].Value + number + m.Groups.["end"].Value
            Regex.Replace(text, @"(?<start>^|\s)(?<number>\d{3}(?:[ \-.])\d{3}(?:[ \-.])\d{4})(?<end>\s|$)", MatchEvaluator(evaluator))

        let processPhoneNumbers = processAmericanPhoneNumbers
        
        let processTimes text =
            let evaluator (m:Match) =
                let hour = int64 m.Groups.["hour"].Value
                let minute = if m.Groups.["minute"].Success then int64 m.Groups.["minute"].Value else 0L
                let minuteWords =
                    match (hour, minute) with
                    | (h, 0L) when h > 12L -> "hundred hours"
                    | (_, 0L) -> ""
                    | (_, m) when m < 10L -> $"o {toWords m}"
                    | (_, m) -> $" {toWords m}"
                let between = m.Groups.["between"].Value
                let meridiem = 
                    m.Groups.["meridiem"].Value.ToLower()
                    |> Seq.map string 
                    |> String.concat " " 
                    |> fun x -> if x = "" then "" else $" {x}"
                let time = $"{toWords hour} {minuteWords}{between}{meridiem}"
                Regex.Replace(time, @"\s{2,}", " ")

            Regex.Replace(text, bindRegex timeRegex, MatchEvaluator(evaluator))
                
    let rec private processEmojis' acc =
        function
        | ""    -> List.rev acc |> String.concat ""
        | text  ->
            let (length, value) = Option.defaultValue (1, text.[ .. 0]) (Emojis.parseForEmoji text)
            processEmojis' (value :: acc) text.[ length .. ]

    let private processEmojis =  processEmojis' []
    let private processNumbers = 
        Numbers.processPhoneNumbers >>
        Numbers.processAbbreviations >>
        Numbers.processRanges >>
        Numbers.processTimes >>
        Numbers.processOrdinals >>
        Numbers.replaceNumbers
        
    let private simpleSubstitution = 
        Punctuation.simpleReplacement
        |> Seq.foldBack (fun (key, replacement) state -> Regex.Replace(state, key, $" {replacement} "))

    let private simpleRemoval = String.map (fun c -> if List.contains c Punctuation.removal then ' ' else c)

    

    //Ensure that simple substitution and simple removal are the last two functions called when processing text
    //Not doing so may lead to some unexpected incorrect processing, especially with ranges that require the "-" character
    let processSpeakText = processEmojis >> processNumbers >> simpleSubstitution >> simpleRemoval >> normalize

    let rec removeBeginningReplies text repliedTo =
        let pattern =
            List.map (sprintf "@%s") repliedTo
            |> String.concat "|"
            |> sprintf @"^((?:%s)\s+)"
        let nextText = Regex.Replace(text, pattern, "")
        if text = nextText then text else removeBeginningReplies nextText repliedTo
        


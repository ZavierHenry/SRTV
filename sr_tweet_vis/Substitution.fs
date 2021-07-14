namespace SRTV

module Substitution =
    open System
    open System.Text.RegularExpressions

    open Humanizer

    module Punctuation =
        let replace = ""

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
        let toWords (number:int64) = Humanizer.NumberToWordsExtension.ToWords number |> fun x -> x.Replace("-", " ")
        let toOrdinalWords (number:int) =  Humanizer.NumberToWordsExtension.ToOrdinalWords number |> fun x -> x.Replace("-", " ")

        //TODO: change function to change decimals without a leading number (e.g .75)
        let processDecimals text =
            let evaluator (m:Match) =
                let integral = int64 m.Groups.[2].Value |> toWords
                let fractional = Seq.toList m.Groups.[3].Value |> List.map (string >> int64 >> toWords)
                $"""%s{m.Groups.[1].Value}%s{integral} point %s{String.concat " " fractional}"""
            Regex.Replace(text, @"(^|\s)(\d+)\.(\d+)", MatchEvaluator(evaluator)).Replace("-", "")

        let processOrdinals text =
            let evaluator (m:Match) =
                sprintf "%s%s%s%s"
                    <| m.Groups.["start"].Value
                    <| if m.Groups.["startNumber"].Success then (int64 >> toWords) m.Groups.["startNumber"].Value else ""
                    <| (int >> toOrdinalWords) m.Groups.["endNumber"].Value
                    <| m.Groups.["end"].Value
            Regex.Replace(text, @"(?<start>^|\s)(?<startNumber>\d*?)(?:(?<endNumber>\d?1)st|(?<endNumber>\d?2)nd|(?<endNumber>\d?3)rd|(\d?[04-9])th)(?<end>\s|$)", MatchEvaluator(evaluator))

    let rec private processEmojis' acc =
        function
        | ""    -> List.rev acc |> String.concat ""
        | text  ->
            let (length, value) = Option.defaultValue (1, text.[ .. 0]) (Emojis.parseForEmoji text)
            processEmojis' (value :: acc) text.[ length .. ]

    let private processEmojis = processEmojis' []
    let private processNumbers = 
        Numbers.processDecimals >> Numbers.processOrdinals

    let processSpeakText = processEmojis >> processNumbers


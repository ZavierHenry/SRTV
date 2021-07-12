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
        let toWords : int64 -> string = Humanizer.NumberToWordsExtension.ToWords

        let processDecimals text =
            let evaluator (m:Match) =
                let integral = int64 m.Groups.[2].Value |> toWords
                let fractional = Seq.toList m.Groups.[3].Value |> List.map (string >> int64 >> toWords)
                $"""%s{m.Groups.[1].Value}%s{integral} point %s{String.concat " " fractional}"""
            Regex.Replace(text, @"(^|\s)(\d+)\.(\d+)", MatchEvaluator(evaluator))

    let rec private processEmojis' acc =
        function
        | ""    -> List.rev acc |> String.concat ""
        | text  ->
            let (length, value) = Option.defaultValue (1, text.[ .. 0]) (Emojis.parseForEmoji text)
            processEmojis' (value :: acc) text.[ length .. ]

    let private processEmojis = processEmojis' []
    let private processNumbers = Numbers.processDecimals

    let processSpeakText = processEmojis >> processNumbers


namespace SRTV

module Substitution =
    open System

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


        let rec tryFindNext text (Node (_, v, _) as node) =
            match text with
            | ""    -> v
            | t     ->
                let endIndex = if Char.IsSurrogatePair(text, 0) then 1 else 0
                let key = t.[ .. endIndex ]
                Trie.tryFindChild key node
                |> Option.bind (tryFindNext text.[endIndex .. ])
                |> Option.orElse v

        let rec add codepoints value (Node (k1, v1, children) as node) =
            match codepoints with
            | ""    ->  Node (k1, Some value, children)
            | cs    ->
                let endIndex = if Char.IsSurrogatePair(codepoints, 0) then 1 else 0
                let key = cs.[ .. endIndex ]
                let child =
                    Trie.tryFindChild key node 
                    |> Option.defaultValue (Node (key, None, []))
                    |> add codepoints.[ endIndex + 1 .. ] value
                
                Node (k1, v1, List.distinctBy Trie.key<string> (child::children))

        let private emojiTrie = 
            Emoji.GetSample().Rows 
            |> Seq.fold (fun trie emoji -> add emoji.Emoji emoji.Name trie) Trie<string>.emptyTrie

        let parseForEmoji text = tryFindNext text emojiTrie

    module Numbers =
        let singleDigits = [
            "zero"
            "one"
            "two"
            "three"
            "four"
            "five"
            "six"
            "seven"
            "eight"
            "nine"
        ]

    let rec private processEmojis' acc =
        function
        | ""    -> List.rev acc |> String.concat ""
        | text  ->
            let value = Option.defaultValue text.[ .. 0] (Emojis.parseForEmoji text)
            processEmojis' (value :: acc) text.[ value.Length .. ]

    let private processEmojis = processEmojis' []

    let processSpeakText = processEmojis


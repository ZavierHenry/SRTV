namespace SRTV

module Substitution =
    module Punctuation =
        let replacements = 
            seq {
                ("#", "hashtag");
                ("%", "percent")
                ("_", "underscore")
            } 
            |> Map

    module Emojis =
        let x = 11

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


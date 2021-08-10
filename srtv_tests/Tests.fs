module Tests

open System

open Xunit
open FsUnit.Xunit

open SRTV.TweetMedia
open SRTV.Substitution

open FSharp.Data

open NHamcrest.Core

open Newtonsoft.Json.Schema
open Newtonsoft.Json.Linq

open System.Collections.Generic
open System.Text.RegularExpressions

open Humanizer

let [<Literal>] testRepository = "https://raw.githubusercontent.com/ZavierHenry/SRTV-test-tweet-collection/main/"
let [<Literal>] tweetsDirectory = testRepository + "tweets/"
let [<Literal>] examplesListFile = testRepository + "exampleFilepaths.txt"

let [<Literal>] samplesFile = testRepository + "samples.json"
let [<Literal>] schemaFile = testRepository + "schema.json"
let [<Literal>] templateFile = "http://json-schema.org/draft-07/schema"

type TestTweet = JsonProvider<samplesFile, SampleIsList=true, InferTypesFromValues=false>
type SchemaTemplate = JsonProvider<templateFile>
type TestTweetSchema = JsonProvider<schemaFile>

module Matchers = 

    type SchemaMatcher(sample:string) = 
        inherit Matcher<obj>()

        let schema = JSchema.Parse <| sample
        let mutable messages = List() :> IList<string>

        override this.DescribeTo(description) =
            description.AppendText("JSON should validate against this schema") |> ignore

        override this.Matches(item:obj) =
            let token = item :?> JsonValue
            let token = JToken.Parse <| token.ToString()
            token.IsValid(schema, &messages)

        override this.DescribeMismatch(item, mismatchDescription) =
            Seq.iter (fun err -> mismatchDescription.AppendText(err + "\n\n") |> ignore) messages

        static member matchSchema(value:JsonValue) = SchemaMatcher <| value.ToString()

    let inline matchSchema (schema:TestTweetSchema.Root) = SchemaMatcher.matchSchema(schema.JsonValue)
    let inline matchTemplate (template:SchemaTemplate.Root) = SchemaMatcher.matchSchema(template.JsonValue)

    type PatternMatcher(pattern:string) =
        inherit Matcher<obj>()

        let regex = Regex(pattern)

        override this.DescribeTo(description) =
            description.AppendText($"String should match the pattern {pattern}") |> ignore

        override this.Matches(item:obj) = Regex.IsMatch(item :?> string, pattern)

        static member matchPattern(pattern:string) = PatternMatcher(pattern)

    let inline matchPattern pattern = PatternMatcher.matchPattern pattern

    type SubstitutionMatcher(original:string, replacement:string) =
        inherit AllOfMatcher<obj>(seq { haveSubstring replacement; not' (haveSubstring original) })

        override this.DescribeTo(description) =
            description.AppendText($"{replacement} to replace {original} in the text") |> ignore

        static member haveSubstitution(original:string, replacement:string) = SubstitutionMatcher(original, replacement)

    //tests whether the left has been substituted with the right
    let inline haveSubstitution (original, replacement) = SubstitutionMatcher.haveSubstitution(original, replacement)
            
let pollToMedia (poll:TestTweet.Poll) =
    let options = 
        poll.Options
        |> Array.map (fun opt -> (opt.Option, int opt.Votes))
        |> Array.toList
    Poll ( options, DateTime.Parse (poll.EndDate) )

let urlCardToMedia (card:TestTweet.UrlCard) =
    Card (card.Title, card.Description, card.Url)


let altTextToMedia f (altText:TestTweet.ImageAltText) : Media =
    f <| Option.filter (fun _ -> altText.HasAltText) altText.AltText

let attributionToMedia (attribution:TestTweet.VideoAttribution) : Media =
    Video <| Option.filter (fun _ -> attribution.HasAttribution) attribution.Attribution

let toQuotedTweet (quotedTweet:TestTweet.QuotedTweet) =
    let toMedia (f:'a -> Media) = Option.toList << Option.map f

    quotedTweet.Tweet
    |> Option.filter (fun _ -> quotedTweet.Available)
    |> Option.map (fun tweet ->
        let images = tweet.ImageAltTexts |> Array.map (altTextToMedia Image) |> Array.toList
        let gif = toMedia (altTextToMedia Gif) tweet.GifAltText
        let video = toMedia attributionToMedia tweet.VideoAttribution

        Tweet (
            tweet.Author.ScreenName,
            tweet.Author.Name,
            tweet.Author.Verified,
            tweet.Author.Protected,
            DateTime.Parse(tweet.DateCreated),
            Array.toList tweet.RepliedTo,
            tweet.Text,
            images @ gif @ video,
            tweet.HasPoll ))
    |> Option.defaultValue Unavailable

let toMockTweet(root:TestTweet.Root) =
    let tweet = root.Tweet
    let toMedia (f:'a -> Media) = Option.toList << Option.map f
    
    let poll = toMedia pollToMedia tweet.Poll
    let card = toMedia urlCardToMedia tweet.UrlCard
    let gif = toMedia (altTextToMedia Gif) tweet.GifAltText
    let video = toMedia attributionToMedia tweet.VideoAttribution

    let images = tweet.ImageAltTexts |> Array.map (altTextToMedia Image) |> Array.toList
    
    MockTweet(
        tweet.Text,
        tweet.Author.ScreenName,
        tweet.Author.Name,
        DateTime.Parse(tweet.DateCreated),
        tweet.Author.Verified,
        tweet.Author.Protected,
        tweet.Retweeter,
        Array.toList tweet.RepliedTo,
        Option.map toQuotedTweet tweet.QuotedTweet,
        images @ video @ gif @ poll @ card
    )

open Matchers

type TestExamples() =

    let tweets = 
        Http.RequestString(examplesListFile)
        |> fun x -> Regex.Split(x, @"\r?\n")
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map ( fun relativeFilepath -> ( relativeFilepath, TestTweet.Load (tweetsDirectory + relativeFilepath) ))
    
    member __.fetch filename = snd <| Array.find (fun (name, _) -> filename = name) tweets
    member __.examples () = Array.map snd tweets |> Seq.ofArray
    member __.filterByFilepath f = Array.filter (fun (name, _) -> f name) tweets |> Array.map snd |> Seq.ofArray

let examples = TestExamples()
let fetchTweet filename = examples.fetch filename
let fetchExamples () = examples.examples ()

let toSpeakText (mockTweet:MockTweet) = mockTweet.ToSpeakText()

let fetchSpeakText = fetchTweet >> toMockTweet >> toSpeakText

let inline noTest () = failwith<unit> "Test has not been implemented as of yet"

let toMemberData data = Seq.map (fun x -> [| x :> obj |]) data

type ``test json schema is valid``() =
    let template = SchemaTemplate.GetSample()
    let schema = TestTweetSchema.GetSample().JsonValue

    [<Fact>]
    member __.``schema is valid``() =
        schema |> should matchTemplate template

type ``test tweets are valid examples``() =
    static member examples () = toMemberData <| fetchExamples ()

    [<Theory>]
    [<MemberData(nameof(``test tweets are valid examples``.examples))>]
    member __.``examples are valid``(testTweet:TestTweet.Root) =
        let schema = TestTweetSchema.GetSample()
        testTweet.JsonValue |> should matchSchema schema

type ``tweet times are properly shown``() =

    [<Fact>]
    member __.``tweet times are correctly displayed``() =
        noTest ()

type ``verified tweets are properly parsed``() =

    static member verifiedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> testTweet.Tweet.Author.Verified)
        |> toMemberData

    static member unverifiedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> not testTweet.Tweet.Author.Verified)
        |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``verified tweets are properly parsed``.verifiedTweets))>]
    member __.``speak text should indicate that the author is verified for verified accounts``(testTweet:TestTweet.Root) =
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should haveSubstring "verified account"

    [<Theory>]
    [<MemberData(nameof(``verified tweets are properly parsed``.unverifiedTweets))>]
    member __.``speak test should NOT indicate that the author is verified for unverified accounts``(testTweet:TestTweet.Root) =
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should not' (haveSubstring "verified account")

type ``tweets from protected accounts are properly parsed``() =

    static member protectedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> testTweet.Tweet.Author.Protected)
        |> toMemberData

    static member unprotectedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> not testTweet.Tweet.Author.Protected)
        |> toMemberData
    
    [<Theory>]
    [<MemberData(nameof(``tweets from protected accounts are properly parsed``.protectedTweets))>]
    member __.``speak text should indicate that the author is private for private accounts``(testTweet:TestTweet.Root) =
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should haveSubstring "protected account"

    [<Theory>]
    [<MemberData(nameof(``tweets from protected accounts are properly parsed``.unprotectedTweets))>]
    member __.``speak text should NOT indicate that that author is private for public accounts``(testTweet:TestTweet.Root) =
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should not' (haveSubstring "protected account")

type ``poll tweets are properly parsed``() =

    [<Theory>]
    [<InlineData("poll.json")>]
    member __.``Finished polls should indicate that they are finished``(filepath) =
        let speakText = fetchSpeakText filepath
        speakText.ToLower() |> should haveSubstring "final results"

    [<Theory>]
    [<InlineData(420, "7 minutes left")>]
    [<InlineData(15, "15 seconds left")>]
    [<InlineData(10800, "3 hours left")>]
    [<InlineData(432000, "5 days left")>]
    member __.``Unfinished polls should indicate the time they have left``(time:int, expected:string) =
        let mockTweet = toMockTweet (fetchTweet "poll.json")
        let date = DateTime.UtcNow
        let newMockTweet = 
            MockTweet(
                mockTweet.Text, 
                mockTweet.ScreenName, 
                mockTweet.Name, 
                date.AddSeconds(float time), 
                mockTweet.IsVerified, 
                mockTweet.IsProtected, 
                mockTweet.Retweeter, 
                mockTweet.RepliedTo,
                mockTweet.QuotedTweet,
                mockTweet.Media
            )
        let speakText = newMockTweet.ToSpeakText()
        speakText |> should haveSubstring expected

    [<Fact>]
    member __.``Finished polls should display the options and percentages of votes``() =
        noTest ()

    [<Fact>]
    member __.``Polls should output the list of options``() =
        let testTweet = fetchTweet "poll.json"
        let speakText = (toMockTweet testTweet).ToSpeakText()
        
        testTweet.Tweet.Poll.Value.Options
        |> Seq.map (fun opt -> processSpeakText opt.Option)
        |> Seq.iter (fun opt -> speakText |> should haveSubstring opt)

    [<Fact>]
    member __.``Unfinished polls should not have the words "final results"``() =
        let mockTweet = toMockTweet (fetchTweet "poll.json")
        let date = DateTime.UtcNow
        let newMockTweet = 
            MockTweet(
                mockTweet.Text, 
                mockTweet.ScreenName, 
                mockTweet.Name, 
                date.AddSeconds(5.0), 
                mockTweet.IsVerified, 
                mockTweet.IsProtected, 
                mockTweet.Retweeter, 
                mockTweet.RepliedTo, 
                mockTweet.QuotedTweet,
                mockTweet.Media
            )
        let speakText = newMockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring "final results")

type ``image tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("imageTweetNoAltText.json")>]
    member __.``images without alt text output the word "image"``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring "image"

    [<Theory>]
    [<InlineData("imagesAltText.json")>]
    member __.``images with alt text show the alt text``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        testTweet.Tweet.ImageAltTexts
        |> Array.filter ( fun x -> x.HasAltText )
        |> Array.map (fun x -> Option.get x.AltText)
        |> Array.iter (fun altText -> speakText |> should haveSubstring (processSpeakText altText))

type ``video tweets are properly parsed`` () =

    [<Theory>]
    [<InlineData("videoAttribution.json")>]
    member __.``videos with attribution display that attribution``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        let attribution = testTweet.Tweet.VideoAttribution |> Option.get |> fun x -> Option.get x.Attribution
        speakText |> should haveSubstring attribution

type ``gif tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("gifNoAltText.json")>]
    member __.``GIFs without alt text output the words "embedded video gif"``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring "embedded video gif"

    [<Theory>]
    [<InlineData("gifAltText.json")>]
    member __.``GIFs with alt text show the alt text``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        let altText =
            testTweet.Tweet.GifAltText
            |> Option.get
            |> fun x -> x.AltText
            |> Option.get
        speakText |> should haveSubstring altText


type ``replies are properly parsed``() =
    
    [<Theory>]
    [<InlineData("basicReply.json")>]
    [<InlineData("twoReplyingTo.json")>]
    [<InlineData("threeReplyingTo.json")>]
    [<InlineData("fourReplyingTo.json")>]
    [<InlineData("sevenReplyingTo.json")>]
    member __.``replies properly show the screen names of the accounts being replied to``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = toMockTweet testTweet |> toSpeakText

        speakText |> should haveSubstring "Replying to"

        let repliedToPattern = 
            testTweet.Tweet.RepliedTo
            |> Array.map ( fun x -> ($"@{x}" |> processSpeakText |> sprintf "%s\s+").TrimStart() )
            |> String.concat "|"
            |> sprintf "(%s)"

        let pattern = 
            $@"Replying to (?<first>{repliedToPattern})(((?<second>{repliedToPattern}))?and ((?<third>{repliedToPattern})|(?<others>.*?) others)?)?"

        speakText |> should matchPattern pattern
        let m = Regex.Match(speakText, pattern)

        let first = m.Groups.["first"]
        let second = m.Groups.["second"]
        let third = m.Groups.["third"]
        let others = m.Groups.["others"]

        first.Success |> should be True

        match testTweet.Tweet.RepliedTo.Length with
        | 0 | 1 -> ()
        | 2 ->
            second.Success |> should be False
            third.Success |> should be True
            others.Success |> should be False
            third.Value |> should not' (equal first.Value)
        | 3 ->
            second.Success |> should be True
            third.Success |> should be True
            others.Success |> should be False
            second.Value |> should not' (equal first.Value)
            third.Value |> should not' (equal first.Value)
            second.Value |> should not' (equal third.Value)
        | n -> 
            second.Success |> should be True
            third.Success |> should be False
            others.Success |> should be True
            second.Value |> should not' (equal first.Value)
            others.Value |> should equal ( (n-2) .ToWords() )


    [<Theory>]
    [<InlineData("basicReply.json")>]
    [<InlineData("twoReplyingTo.json")>]
    [<InlineData("threeReplyingTo.json")>]
    [<InlineData("fourReplyingTo.json")>]
    [<InlineData("sevenReplyingTo.json")>]
    member __.``beginning replies are removed from the tweet text``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = toMockTweet testTweet |> toSpeakText
        
        let repliedToPattern = 
            testTweet.Tweet.RepliedTo
            |> Array.map (sprintf "@%s\s+")
            |> String.concat "|"
            |> sprintf "(%s)"

        let restText = Regex.Replace(testTweet.Tweet.Text, $@"^{repliedToPattern}+", "")
        let pattern =
            testTweet.Tweet.RepliedTo
            |> Array.map (sprintf "@%s" >> processSpeakText >> sprintf "%s\s+")
            |> String.concat "|"
            |> sprintf "(%s)"
            |> sprintf @"Replying to %s( (%s )?and (%s|\d+ others))?"

        speakText |> should haveSubstring (processSpeakText restText)

        //TODO: make sure test is not just a false positive
        speakText |> should not' (matchPattern <| $"{pattern} {Regex.Escape <| processSpeakText testTweet.Tweet.Text}")


type ``retweets are properly parsed``() =

    [<Theory>]
    [<InlineData("retweet.json")>]
    member __.``retweets properly show the name of the account retweeting the tweet``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should haveSubstring $"{Option.get testTweet.Tweet.Retweeter |> processSpeakText} retweeted"

type ``quoted tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("quotedTweet.json")>]
    member __.``quoted tweets should be shown``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = toMockTweet testTweet |> toSpeakText
        let quotedTweet = Option.get testTweet.Tweet.QuotedTweet |> fun x -> Option.get x.Tweet

        let quotedSpeakText = 
            MockTweet (
                quotedTweet.Text, 
                quotedTweet.Author.ScreenName, 
                quotedTweet.Author.Name, 
                DateTime.Parse(quotedTweet.DateCreated), 
                quotedTweet.Author.Verified, 
                quotedTweet.Author.Protected, 
                None, 
                Array.toList quotedTweet.RepliedTo, 
                None,
                Seq.empty)
            |> toSpeakText
        speakText |> should haveSubstring quotedSpeakText


    [<Theory>]
    [<InlineData("quotedTweet.json")>]
    member __.``there should be an indication that a tweet is being quoted``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring "quote tweet"

    [<Fact>]
    member __.``quoted tweets with polls in the quote tweet should be indicated``() =
        noTest ()

type ``numbers are properly converted to words``() =
    
    static let startsWith (prefix:string) (str:string) = str.StartsWith(prefix)

    static member dates () = toMemberData <| examples.filterByFilepath (startsWith "numbers/dates/")
    static member times () = toMemberData <| examples.filterByFilepath (startsWith "numbers/times/")
    static member ordinals () = toMemberData <| examples.filterByFilepath (startsWith "numbers/ordinals/")
    static member currency () = toMemberData <| examples.filterByFilepath (startsWith "numbers/currency/")
    static member abbreviations () = toMemberData <| examples.filterByFilepath (startsWith "numbers/abbreviations/")

    [<Theory>]
    [<InlineData("numbers/phoneNumberOnePlus.json")>]
    [<InlineData("numbers/phoneNumberDots.json")>]
    member __.``phone numbers are converted to words properly``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = toSpeakText <| toMockTweet testTweet

        for replacement in testTweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<InlineData("numbers/negativeNumber.json")>]
    member __.``numbers under zero include "minus"``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution ("-", "minus")

    [<Theory>]
    [<InlineData("numbers/decimalPercentage.json", "67.94", "sixty seven point nine four")>]
    [<InlineData("numbers/decimalPercentage.json", "58.41", "fifty eight point four one")>]
    member __.``decimal numbers (e.g. 3.45) are converted to the form "three point four five"``(filepath:string, decimal:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (decimal, expected)

    [<Theory>]
    [<InlineData("numbers/numberWithComma.json")>]
    [<InlineData("numbers/13000.json")>]
    member __.``whole numbers are converted to word form``(filepath: string) =
        let testTweet = fetchTweet filepath
        let speakText = toMockTweet testTweet |> toSpeakText
        for replacement in testTweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.ordinals))>]
    member __.``ordinal numbers (e.g. 2nd) are converted to word form``(testTweet:TestTweet.Root) =
        let speakText = toMockTweet testTweet |> toSpeakText
        for replacement in testTweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<InlineData("numbers/year.json", "2008", "two thousand eight")>]
    member __.``obvious years are properly converted to words``(filepath:string, year:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (year, expected)

    [<Fact>]
    member __.``number ranges are converted to words``() =
        noTest ()

    [<Fact>]
    member __.``addresses are properly converted to words``() =
        noTest ()

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.times))>]
    member __.``times are converted to words``(tweet:TestTweet.Root) =
        let speakText = toSpeakText <| toMockTweet tweet
        for replacement in tweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.currency))>]
    member __.``currency is converted to words``(tweet:TestTweet.Root) =
        let speakText = toSpeakText <| toMockTweet tweet
        for replacement in tweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.dates))>]
    member __.``obvious dates are converted into words``(tweet:TestTweet.Root) =
        let speakText = toSpeakText <| toMockTweet tweet
        for replacement in tweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Fact>]
    member __.``date ranges are converted into words``() =
        noTest ()

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.abbreviations))>]
    member __.``abbreviations are converted to words``(tweet:TestTweet.Root) =
        let speakText = toSpeakText <| toMockTweet tweet
        for replacement in tweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

type ``emojis are properly converted to words``() =

    static member emojis () = examples.filterByFilepath (fun x -> x.StartsWith("emojis/")) |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``emojis are properly converted to words``.emojis))>]
    member __.``Emojis should have correct speak text``(tweet:TestTweet.Root) =
        let speakText = toMockTweet tweet |> toSpeakText
        for replacement in tweet.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

        
type ``punctuation is properly converted to words``() = 

    [<Theory>]
    [<InlineData("punctuation/hashtag.json", "#", "hashtag")>]
    [<InlineData("punctuation/underscore.json", "_", "underscore")>]
    [<InlineData("punctuation/percent.json", "%", "percent")>]
    [<InlineData("punctuation/mathEquation.json", "=", "equals")>]
    [<InlineData("punctuation/mathEquation.json", "^", "caret")>]
    [<InlineData("punctuation/atSymbol.json", "@", "at")>]
    member __.``symbols that should be replaced are properly replaced``(filepath:string, symbol:string, replacement:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (symbol, replacement)

    [<Fact>]
    member __.``periods properly indicate a long pause``() =
        noTest ()

    [<Theory>]
    [<InlineData("urlCard.json")>]
    [<InlineData("imagesAltText.json")>]
    [<InlineData("multipleTcoLinks.json")>]
    member __.``t.co links are removed from the tweet``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should not' (haveSubstring "t.co/")

    [<Fact>]
    member __.``symbols that should be removed are properly removed``() =
        noTest ()
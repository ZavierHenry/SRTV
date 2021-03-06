module Tests

open System

open Xunit
open FsUnit.Xunit
open Xunit.Abstractions

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
    let host = Uri(card.UnwoundUrl).Host
    Card (card.Url, card.Title, card.Description, host)

let altTextToMedia f (altText:TestTweet.ImageAltText) : Media =
    f <| Option.filter (fun _ -> altText.HasAltText) altText.AltText

let attributionToMedia (attribution:TestTweet.VideoAttribution) : Media =
    Video <| Option.filter (fun _ -> attribution.HasAttribution) attribution.Attribution


let toUrlType = function 
    | "regular" -> UrlType.Regular 
    | "quoteTweet" -> UrlType.QuoteTweet 
    | "media" -> UrlType.Media 
    | _ -> UrlType.Regular

let toQuotedTweet (quotedTweet:TestTweet.QuotedTweet) =
    let toMedia (f:'a -> Media) = Option.toList << Option.map f

    quotedTweet.Tweet
    |> Option.filter (fun _ -> quotedTweet.Available)
    |> Option.map (fun tweet ->
        let images = tweet.ImageAltTexts |> Array.map (altTextToMedia Image) |> Array.toList
        let gif = toMedia (altTextToMedia Gif) tweet.GifAltText
        let video = toMedia attributionToMedia tweet.VideoAttribution
        let urls = 
            tweet.Urls 
            |> Array.map (fun url -> Url (url.Url, url.DisplayUrl, toUrlType url.Type))

        Tweet (
            tweet.Author.ScreenName,
            tweet.Author.Name,
            tweet.Author.Verified,
            tweet.Author.Protected,
            DateTime.Parse(tweet.DateCreated),
            Array.toList tweet.RepliedTo,
            tweet.Text,
            images @ gif @ video,
            urls,
            tweet.HasPoll ))
    |> Option.defaultValue Unavailable

let toMockTweet(root:TestTweet.Root) =
    let tweet = root.Tweet
    let toMedia (f:'a -> Media) = Option.toList << Option.map f
    
    let poll = toMedia pollToMedia tweet.Poll
    let cards = Array.map urlCardToMedia tweet.UrlCards |> Array.toList
    let gif = toMedia (altTextToMedia Gif) tweet.GifAltText
    let video = toMedia attributionToMedia tweet.VideoAttribution

    let images = tweet.ImageAltTexts |> Array.map (altTextToMedia Image) |> Array.toList
    let urls = tweet.Urls |> Array.map (fun url -> Url (url.Url, url.DisplayUrl, toUrlType url.Type))
    
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
        images @ video @ gif @ poll @ cards,
        urls
    )

open Matchers


type SerializableTestTweet(tweet:TestTweet.Root) =
    
    [<Literal>]
    static let infoKey = "tweet"

    member val Value = tweet with get, set

    new() = SerializableTestTweet(TestTweet.GetSamples().[0])

    member this.ToMockTweet() = toMockTweet this.Value
    member this.ToSpeakText() = this.ToMockTweet().ToSpeakText()
    member this.ToFullSpeakText() = this.ToMockTweet().ToFullSpeakText()

    interface IXunitSerializable with
        member this.Serialize(info) = info.AddValue( infoKey, this.Value.ToString() )
        member this.Deserialize(info) = this.Value <- info.GetValue(infoKey) |> TestTweet.Parse

type TestExamples() =

    let tweets = 
        Http.RequestString(examplesListFile)
        |> fun x -> Regex.Split(x, @"\r?\n")
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map ( fun relativeFilepath -> ( relativeFilepath, SerializableTestTweet( TestTweet.Load (tweetsDirectory + relativeFilepath) ) ))
    
    member __.fetch filename = snd <| Array.find (fun (name, _) -> filename = name) tweets
    member __.examples () = Array.map snd tweets |> Seq.ofArray
    member __.filterByFilepath f = Array.filter (fun (name, _) -> f name) tweets |> Array.map snd |> Seq.ofArray

let examples = TestExamples()
let fetchTweet filename = examples.fetch filename
let fetchExamples () = examples.examples ()

let toSpeakText (mockTweet:MockTweet) = mockTweet.ToSpeakText()
let toFullSpeakText (mockTweet:MockTweet) = mockTweet.ToFullSpeakText()

let fetchSpeakText filename = (fetchTweet filename).ToSpeakText()
let fetchFullSpeakText filename = (fetchTweet filename).ToFullSpeakText()

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
    member __.``examples are valid``(testTweet:SerializableTestTweet) =
        let schema = TestTweetSchema.GetSample()
        testTweet.Value.JsonValue |> should matchSchema schema

type ``tweet times are properly shown``() =

    [<Theory>]
    [<InlineData(14400, "four hours ago")>]
    [<InlineData(2640, "forty four minutes ago")>]
    [<InlineData(259200, "three days ago")>]
    member __.``tweet times are correctly displayed``(seconds:int, expected:string) =
        let mockTweet = (fetchTweet "basicVerifiedTweet.json").ToMockTweet()
        let now = DateTime.UtcNow
        let newDate = now.AddSeconds(float -seconds)
        let otherMockTweet =
            MockTweet (
                mockTweet.Text,
                mockTweet.ScreenName,
                mockTweet.Name,
                newDate,
                mockTweet.IsVerified,
                mockTweet.IsProtected,
                mockTweet.Retweeter,
                mockTweet.RepliedTo,
                mockTweet.QuotedTweet,
                mockTweet.Media,
                []
            )

        let speakText = otherMockTweet.ToFullSpeakText(now)
        speakText |> should haveSubstring expected


    [<Fact>]
    member __.``Quote tweet times are correctly displayed``() =
        noTest ()


type ``verified tweets are properly parsed``() =

    static member verifiedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> testTweet.Value.Tweet.Author.Verified)
        |> toMemberData

    static member unverifiedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> not testTweet.Value.Tweet.Author.Verified)
        |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``verified tweets are properly parsed``.verifiedTweets))>]
    member __.``speak text should indicate that the author is verified for verified accounts``(testTweet:SerializableTestTweet) =
        let speakText = testTweet.ToFullSpeakText()
        speakText |> should haveSubstring "verified account"

    [<Theory>]
    [<MemberData(nameof(``verified tweets are properly parsed``.unverifiedTweets))>]
    member __.``speak test should NOT indicate that the author is verified for unverified accounts``(testTweet:SerializableTestTweet) =
        let speakText = testTweet.ToFullSpeakText()
        speakText |> should not' (haveSubstring "verified account")

type ``tweets from protected accounts are properly parsed``() =

    static member protectedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> testTweet.Value.Tweet.Author.Protected)
        |> toMemberData

    static member unprotectedTweets () =
        fetchExamples ()
        |> Seq.filter (fun testTweet -> not testTweet.Value.Tweet.Author.Protected)
        |> toMemberData
    
    [<Theory>]
    [<MemberData(nameof(``tweets from protected accounts are properly parsed``.protectedTweets))>]
    member __.``speak text should indicate that the author is private for private accounts``(testTweet:SerializableTestTweet) =
        let speakText = testTweet.ToFullSpeakText()
        speakText |> should haveSubstring "protected account"

    [<Theory>]
    [<MemberData(nameof(``tweets from protected accounts are properly parsed``.unprotectedTweets))>]
    member __.``speak text should NOT indicate that that author is private for public accounts``(testTweet:SerializableTestTweet) =
        let speakText = testTweet.ToFullSpeakText()
        speakText |> should not' (haveSubstring "protected account")

type ``poll tweets are properly parsed``() =

    [<Theory>]
    [<InlineData("poll.json")>]
    member __.``Finished polls should indicate that they are finished``(filepath) =
        let speakText = fetchSpeakText filepath
        speakText.ToLower() |> should haveSubstring "final results"

    [<Theory>]
    [<InlineData(420, "seven minutes left")>]
    [<InlineData(15, "fifteen seconds left")>]
    [<InlineData(10800, "three hours left")>]
    [<InlineData(432000, "five days left")>]
    member __.``Unfinished polls should indicate the time they have left``(time:int, expected:string) =
        let mockTweet = (fetchTweet "poll.json").ToMockTweet()
        let currentPoll = Seq.find (function | Poll _ -> true | _ -> false) mockTweet.Media
        let now = DateTime.UtcNow
        let newPoll = 
            match currentPoll with
            | Poll (options, _) -> Poll (options, float time |> now.AddSeconds)
            | _                 -> currentPoll

        let newMockTweet = 
            MockTweet(
                mockTweet.Text, 
                mockTweet.ScreenName, 
                mockTweet.Name, 
                mockTweet.Date, 
                mockTweet.IsVerified, 
                mockTweet.IsProtected, 
                mockTweet.Retweeter, 
                mockTweet.RepliedTo,
                mockTweet.QuotedTweet,
                seq { yield! Seq.except [currentPoll] mockTweet.Media; newPoll },
                []
            )
        let speakText = newMockTweet.ToSpeakText(now)
        speakText |> should haveSubstring expected

    [<Theory>]
    [<InlineData("poll.json")>]
    member __.``Finished polls should display the options and percentages of votes``(filepath) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToSpeakText()

        let poll = Option.get testTweet.Value.Tweet.Poll
        let total = poll.Options |> Array.sumBy (fun x -> x.Votes) |> decimal
        
        for option in poll.Options do
            let percentage = sprintf "%.1f%%" (option.Votes / total * 100m)
            speakText |> should haveSubstring (processSpeakText option.Option)
            speakText |> should haveSubstring (processSpeakText percentage)


    [<Fact>]
    member __.``Polls should output the list of options``() =
        let testTweet = fetchTweet "poll.json"
        let speakText = testTweet.ToSpeakText()
        
        testTweet.Value.Tweet.Poll.Value.Options
        |> Seq.map (fun opt -> processSpeakText opt.Option)
        |> Seq.iter (fun opt -> speakText |> should haveSubstring opt)

    [<Fact>]
    member __.``Unfinished polls should not have the words "final results"``() =
        let mockTweet = (fetchTweet "poll.json").ToMockTweet()
        let currentPoll = Seq.find (function | Poll _ -> true | _ -> false) mockTweet.Media
        let now = DateTime.UtcNow
        let newPoll = 
            match currentPoll with
            | Poll (options, _) -> Poll (options, now.AddSeconds 17.0)
            | _                 -> currentPoll

        let date = DateTime.UtcNow
        let newMockTweet = 
            MockTweet(
                mockTweet.Text, 
                mockTweet.ScreenName, 
                mockTweet.Name, 
                mockTweet.Date, 
                mockTweet.IsVerified, 
                mockTweet.IsProtected, 
                mockTweet.Retweeter, 
                mockTweet.RepliedTo,
                mockTweet.QuotedTweet,
                seq { yield! Seq.except [currentPoll] mockTweet.Media; newPoll },
                []
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
        let speakText = testTweet.ToSpeakText()
        testTweet.Value.Tweet.ImageAltTexts
        |> Array.filter ( fun x -> x.HasAltText )
        |> Array.map (fun x -> Option.get x.AltText)
        |> Array.iter (fun altText -> speakText |> should haveSubstring (processSpeakText altText))

type ``video tweets are properly parsed`` () =

    [<Theory>]
    [<InlineData("videoAttribution.json")>]
    member __.``videos with attribution display that attribution``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToSpeakText()
        let attribution = testTweet.Value.Tweet.VideoAttribution |> Option.get |> fun x -> Option.get x.Attribution
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
        let speakText = testTweet.ToSpeakText()
        let altText =
            testTweet.Value.Tweet.GifAltText
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
        let speakText = testTweet.ToFullSpeakText()

        speakText |> should haveSubstring "Replying to"

        let repliedToPattern = 
            testTweet.Value.Tweet.RepliedTo
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

        match testTweet.Value.Tweet.RepliedTo.Length with
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
        let speakText = testTweet.ToFullSpeakText()
        
        let repliedToPattern = 
            testTweet.Value.Tweet.RepliedTo
            |> Array.map (sprintf "@%s\s+")
            |> String.concat "|"
            |> sprintf "(%s)"

        let restText = Regex.Replace(testTweet.Value.Tweet.Text, $@"^{repliedToPattern}+", "")
        let pattern =
            testTweet.Value.Tweet.RepliedTo
            |> Array.map (sprintf "@%s" >> processSpeakText >> sprintf "%s\s+")
            |> String.concat "|"
            |> sprintf "(%s)"
            |> sprintf @"Replying to %s( (%s )?and (%s|\d+ others))?"

        speakText |> should haveSubstring (processSpeakText restText)

        //TODO: make sure test is not just a false positive
        speakText |> should not' (matchPattern <| $"{pattern} {Regex.Escape <| processSpeakText testTweet.Value.Tweet.Text}")


type ``retweets are properly parsed``() =

    [<Theory>]
    [<InlineData("retweet.json")>]
    member __.``retweets properly show the name of the account retweeting the tweet``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToFullSpeakText()
        speakText |> should haveSubstring $"{Option.get testTweet.Value.Tweet.Retweeter |> processSpeakText} retweeted"

type ``quoted tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("quotedTweet.json")>]
    member __.``quoted tweets should be shown``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToFullSpeakText()
        let quotedTestTweet = Option.get testTweet.Value.Tweet.QuotedTweet |> fun x -> Option.get x.Tweet
        let toMedia (f:'a -> Media) = Option.toList << Option.map f

        let quotedImages = 
            quotedTestTweet.ImageAltTexts |> Array.map (altTextToMedia Image) |> Array.toList

        let quotedGif =
            toMedia (altTextToMedia Gif) quotedTestTweet.GifAltText

        let quotedVideo =
            toMedia attributionToMedia quotedTestTweet.VideoAttribution

        let quotedSpeakText =
            MockTweet(
                quotedTestTweet.Text,
                quotedTestTweet.Author.ScreenName,
                quotedTestTweet.Author.Name,
                DateTime.Parse quotedTestTweet.DateCreated,
                quotedTestTweet.Author.Verified,
                quotedTestTweet.Author.Protected,
                None,
                quotedTestTweet.RepliedTo |> Array.toList,
                None,
                quotedImages @ quotedGif @ quotedVideo,
                quotedTestTweet.Urls |> Array.map (fun url -> Url (url.Url, url.DisplayUrl, toUrlType url.Type))
            )
            |> toFullSpeakText

        speakText |> should haveSubstring quotedSpeakText
       

    [<Theory>]
    [<InlineData("quotedTweet.json")>]
    member __.``there should be an indication that a tweet is being quoted``(filepath:string) =
        let speakText = fetchFullSpeakText filepath
        speakText |> should haveSubstring "quote tweet"

    [<Theory>]
    [<InlineData("quotedTweetPoll.json")>]
    member __.``quoted tweets with polls in the quote tweet should be indicated``(filepath:string) =
        let speakText = fetchFullSpeakText filepath
        speakText |> should haveSubstring "show this poll"


type ``numbers are properly converted to words``() =
    
    static let startsWith (prefix:string) (str:string) = str.StartsWith(prefix)

    static member dates () = toMemberData <| examples.filterByFilepath (startsWith "numbers/dates/")
    static member times () = toMemberData <| examples.filterByFilepath (startsWith "numbers/times/")
    static member ordinals () = toMemberData <| examples.filterByFilepath (startsWith "numbers/ordinals/")
    static member currency () = toMemberData <| examples.filterByFilepath (startsWith "numbers/currency/")
    static member abbreviations () = toMemberData <| examples.filterByFilepath (startsWith "numbers/abbreviations/")
    static member ranges () = toMemberData <| examples.filterByFilepath (startsWith "numbers/ranges/")

    [<Theory>]
    [<InlineData("numbers/phoneNumberOnePlus.json")>]
    [<InlineData("numbers/phoneNumberDots.json")>]
    member __.``phone numbers are converted to words properly``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToSpeakText()

        for replacement in testTweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<InlineData("numbers/negativeNumber.json")>]
    [<InlineData("numbers/negativeCommaNumber.json")>]
    member __.``numbers under zero include "minus"``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = testTweet.ToSpeakText()

        for replacement in testTweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)



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
        let speakText = testTweet.ToSpeakText()
        for replacement in testTweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.ordinals))>]
    member __.``ordinal numbers (e.g. 2nd) are converted to word form``(testTweet:SerializableTestTweet) =
        let speakText = testTweet.ToSpeakText()
        for replacement in testTweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<InlineData("numbers/year.json", "2008", "two thousand eight")>]
    member __.``obvious years are properly converted to words``(filepath:string, year:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (year, expected)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.ranges))>]
    member __.``number ranges are converted to words``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()

        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Fact>]
    member __.``addresses are properly converted to words``() =
        noTest ()

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.times))>]
    member __.``times are converted to words``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.currency))>]
    member __.``currency is converted to words``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.dates))>]
    member __.``obvious dates are converted into words``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Fact>]
    member __.``date ranges are converted into words``() =
        noTest ()

    [<Theory>]
    [<MemberData(nameof(``numbers are properly converted to words``.abbreviations))>]
    member __.``abbreviations are converted to words``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

type ``emojis are properly converted to words``() =

    static member emojis () = examples.filterByFilepath (fun x -> x.StartsWith("emojis/")) |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``emojis are properly converted to words``.emojis))>]
    member __.``Emojis should have correct speak text``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToFullSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

        
type ``punctuation is properly converted to words``() = 

    static member punctuation () = examples.filterByFilepath(fun x -> x.StartsWith("punctuation/")) |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``punctuation is properly converted to words``.punctuation))>]
    member __.``symbols that should be replaced are properly replaced``(tweet:SerializableTestTweet) =
        let speakText = tweet.ToSpeakText()
        for replacement in tweet.Value.Replacements do
            speakText |> should haveSubstitution (replacement.OldText, replacement.NewText)

    [<Fact>]
    member __.``periods properly indicate a long pause``() =
        noTest ()

    [<Fact>]
    member __.``symbols that should be removed are properly removed``() =
        noTest ()

    [<Theory>]
    [<InlineData("basicReply.json")>]
    [<InlineData("twoReplyingTo.json")>]
    [<InlineData("threeReplyingTo.json")>]
    [<InlineData("fourReplyingTo.json")>]
    [<InlineData("sevenReplyingTo.json")>]
    member __.``beginning replies are removed from the tweet text``(filepath:string) =
        let testTweet = fetchTweet filepath
        let mockTweet = testTweet.ToMockTweet()
        let otherText = Regex.Replace(mockTweet.Text, @"^((?:@\w+\s+)+)", "")

        let otherMockTweet =
            MockTweet (
                otherText,
                mockTweet.ScreenName,
                mockTweet.Name,
                mockTweet.Date,
                mockTweet.IsVerified,
                mockTweet.IsProtected,
                mockTweet.Retweeter,
                mockTweet.RepliedTo,
                mockTweet.QuotedTweet,
                mockTweet.Media,
                mockTweet.Urls
            )

        let speakText = mockTweet.ToSpeakText()
        let otherSpeakText = otherMockTweet.ToSpeakText()

        otherMockTweet.Text |> should not' (equal mockTweet.Text)
        speakText |> should equal otherSpeakText
       

type ``links are properly handled in tweets``() =

    [<Theory>]
    [<InlineData("urlCard.json")>]
    [<InlineData("imagesAltText.json")>]
    [<InlineData("multipleTcoLinks.json")>]
    member __.``tco links are removed from the tweet``(filepath:string) =
        let mockTweet = (fetchTweet filepath).ToMockTweet()
        let speakText = mockTweet.ToSpeakText()

        for Url (url, _, _) in mockTweet.Urls do
            speakText |> should not' (haveSubstring <| processSpeakText url)

    [<Theory>]
    [<InlineData("multipleTcoLinks.json")>]
    member __.``regular links show up as the display url``(filepath:string) =
        let mockTweet = (fetchTweet filepath).ToMockTweet()
        let speakText = mockTweet.ToSpeakText()

        for Url (url, displayUrl, _) in mockTweet.Urls |> Seq.filter (function | Url (_, _, UrlType.Regular) -> true | _ -> false) do
            speakText |> should haveSubstitution (processSpeakText url, processSpeakText displayUrl)

    [<Theory>]
    [<InlineData("imagesAltText.json")>]
    [<InlineData("quotedTweet.json")>]
    [<InlineData("quotedTweetPoll.json")>]
    member __.``media and quote tweet links should NOT have the display url in the text``(filepath:string) =
        let mockTweet = (fetchTweet filepath).ToMockTweet()
        let speakText = mockTweet.ToSpeakText()

        for Url (_, displayUrl, _) in mockTweet.Urls |> Seq.filter (function | Url (_, _, UrlType.Media) | Url (_, _, UrlType.QuoteTweet) -> true | _ -> false) do
            speakText |> should not' (haveSubstring <| processSpeakText displayUrl)


open SRTV.Regex.Urls

open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

type UrlTest() =

    member val Description = "" with get, set
    member val Text = "" with get, set
    member val Expected : string [] = [||] with get, set

    interface IXunitSerializable with
        member this.Serialize(info) = 
            info.AddValue("description", this.Description)
            info.AddValue("text", this.Text)
            info.AddValue("expected", this.Expected)

        member this.Deserialize(info) =
            this.Description <- info.GetValue("description")
            this.Text <- info.GetValue("text")
            this.Expected <- info.GetValue("expected")

type UrlIndicesTestResult() =
    
    member val Url = "" with get, set
    member val Indices : int [] = [||] with get, set

    interface IXunitSerializable with
        member this.Serialize(info) = 
            info.AddValue("url", this.Url)
            info.AddValue("indices", this.Indices)
        
        member this.Deserialize(info) =
            this.Url <- info.GetValue("url")
            this.Indices <- info.GetValue("indices")

type UrlIndicesTest() =
    
    member val Description = "" with get, set
    member val Text = "" with get, set
    member val Expected : UrlIndicesTestResult [] = [||] with get, set

    interface IXunitSerializable with
        member this.Serialize(info) =
            info.AddValue("description", this.Description)
            info.AddValue("text", this.Text)
            info.AddValue("expected", this.Expected)

        member this.Deserialize(info) =
            this.Description <- info.GetValue("description")
            this.Text <- info.GetValue("text")
            this.Expected <- info.GetValue("expected")

type TwitterTests() = 
    
    member val Urls : UrlTest [] = [||] with get, set
    
    [<YamlMember(Alias="urls_with_indices", ApplyNamingConventions = false)>]
    member val UrlsWithIndices : UrlIndicesTest [] = [||] with get, set

    [<YamlMember(Alias="tco_urls_with_params", ApplyNamingConventions = false)>]
    member val TcoUrlsWithParams : UrlTest [] = [||] with get, set

    [<YamlMember(Alias="urls_with_directional_markers", ApplyNamingConventions = false)>]
    member val UrlsWithDirectionalMarkers : UrlIndicesTest [] = [||] with get, set


type TwitterConformance() =
    member val Tests = TwitterTests() with get, set

let deserializer = 
       DeserializerBuilder()
           .WithNamingConvention(CamelCaseNamingConvention.Instance)
           .IgnoreUnmatchedProperties()
           .Build()

let conformanceTests = 
    Http.RequestString("https://raw.githubusercontent.com/twitter/twitter-text/master/conformance/extract.yml")
    |> fun root -> deserializer.Deserialize<TwitterConformance>(root).Tests

type ``extraction of urls are done properly``() =

    static member urlIndicesTests = conformanceTests.UrlsWithIndices |> toMemberData
    static member tcoTestsWithParams = conformanceTests.TcoUrlsWithParams |> toMemberData
    static member urlTests = conformanceTests.Urls |> toMemberData
    static member urlDirectionalMarkersTests = conformanceTests.UrlsWithDirectionalMarkers |> toMemberData

    [<Theory>]
    [<MemberData(nameof(``extraction of urls are done properly``.urlTests))>]
    member __.``urls are extracted``(test:UrlTest) = 
        let expected = test.Expected |> Seq.toList
        let actual = extractUrls test.Text |> Seq.map (fun {url = url} -> url) |> Seq.toList

        actual |> should matchList expected


    [<Theory>]
    [<MemberData(nameof(``extraction of urls are done properly``.urlIndicesTests))>]
    member __.``url extraction has the right indices``(test:UrlIndicesTest) =
        let expected = test.Expected |> Seq.map (fun x -> { url = x.Url; start = x.Indices.[0]}) |> Seq.toList
        let actual = extractUrls test.Text |> Seq.toList

        actual |> should matchList expected
       

    [<Theory>]
    [<MemberData(nameof(``extraction of urls are done properly``.tcoTestsWithParams))>]
    member __.``tco links are properly handled``(test:UrlTest) =
        let expected = test.Expected |> Seq.toList
        let actual = extractUrls test.Text |> Seq.map (fun {url = url} -> url) |> Seq.toList

        actual |> should matchList expected


    [<Theory>]
    [<MemberData(nameof(``extraction of urls are done properly``.urlDirectionalMarkersTests))>]
    member __.``directional markers tests are handled correctly``(test:UrlIndicesTest) =
        let expected = test.Expected |> Seq.map (fun x -> { url = x.Url; start = x.Indices.[0]}) |> Seq.toList
        let actual = extractUrls test.Text |> Seq.toList
        
        actual |> should matchList expected


open LinqToTwitter
open LinqToTwitter.Common
open System.Text.Json
open System.Text.Json.Serialization

type ``mockTweet constructors parse Twitter response correctly``() =
    let directory = "https://raw.githubusercontent.com/ZavierHenry/SRTV-test-tweet-collection/main/responses/"

    let captureObject keyword opener closer text =
        let rec captureObject' (text:string) opener closer openCount index =
            if openCount = 0
            then text.[ .. index - 1]
            else if index >= text.Length
            then text
            else 
                match string text.[ index ] with
                | c when c = opener -> captureObject' text opener closer (openCount + 1) (index + 1)
                | c when c = closer -> captureObject' text opener closer (openCount - 1) (index + 1)
                | _ -> captureObject' text opener closer openCount (index + 1)

        match Regex.Match(text, $"""(?<="{keyword}":\s+){Regex.Escape opener}""") with
        | m when m.Success ->
            captureObject' text.[ m.Index .. ] opener closer 1 1
        | _ -> ""

    let fetchResponse filename = Http.RequestString(directory + filename)

    [<Theory>]
    [<InlineData("basicResponse.json")>]
    [<InlineData("pollResponse.json")>]
    let ``response without extended entities are converted to mockTweet``(filepath) =
        let options = JsonSerializerOptions()
        options.Converters.Add( JsonStringEnumConverter() )

        let captured = 
            fetchResponse filepath 
            |> captureObject "response" "{" "}"

        let tweetQuery : TweetQuery = JsonSerializer.Deserialize(captured, options)

        let tweet = tweetQuery.Tweets.[0]
        let includes = tweetQuery.Includes
        let user = includes.Users.[0]

        let mockTweet = MockTweet(tweet, includes, Seq.empty)
        mockTweet.Date |> should equal tweet.CreatedAt
        mockTweet.IsProtected |> should equal user.Protected
        mockTweet.IsVerified |> should equal user.Verified

        for poll in ( match includes.Polls with | null -> Seq.empty | polls -> seq { yield! polls } ) do
            let options = poll.Options |> Seq.map (fun opt -> (opt.Label, opt.Votes)) |> Seq.toList
            mockTweet.Media |> should contain ( Poll (options, poll.EndDatetime) )

        mockTweet.QuotedTweet |> should equal None
        mockTweet.RepliedTo |> should be Empty
        mockTweet.Retweeter |> should equal None
        mockTweet.ScreenName |> should equal user.Username
        mockTweet.Name |> should equal user.Name
        mockTweet.Text |> should equal tweet.Text

    [<Theory>]
    [<InlineData("imageAltTextResponse.json")>]
    let ``response with extended entities are converted to mockTweet``(filepath) =
        let response = fetchResponse filepath   
        let capturedResponse = captureObject "response" "{" "}" response
        let entities = captureObject "entities" "[" "]" response

        let options = JsonSerializerOptions()
        options.Converters.Add(JsonStringEnumConverter())

        let tweetQuery = JsonSerializer.Deserialize<TweetQuery>(capturedResponse, options)
        let statuses : Status list = 
            JsonSerializer.Deserialize<List<Status>>(entities, options)
            |> Seq.cast<Status>
            |> Seq.toList

        let tweet = tweetQuery.Tweets.[0]
        let includes = tweetQuery.Includes
        let entities = statuses.[0].ExtendedEntities.MediaEntities 

        let mockTweet = MockTweet(tweet, includes, entities)

        mockTweet.Media |> should contain (Image <| Some entities.[0].AltText)

   
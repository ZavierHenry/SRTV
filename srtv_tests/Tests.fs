module Tests

open System

open Xunit
open FsUnit.Xunit

open SRTV.TweetMedia
open SRTV.Utilities
open SRTV.Substitution

open FSharp.Data

open NHamcrest.Core

open Newtonsoft.Json.Schema
open Newtonsoft.Json.Linq

open System.Collections.Generic
open System.Text.RegularExpressions

let [<Literal>] samplesFile = "./samples.json"
let [<Literal>] schemaFile = "./schema.json"
let [<Literal>] templateFile = "http://json-schema.org/draft-04/schema"

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

let gifAltTextToMedia = Gif << tryNonBlankString
let videoAttributionToMedia = Video << tryNonBlankString

let toMockTweet(root:TestTweet.Root) =
    let tweet = root.Tweet
    let toMedia (f:'a -> Media) = Option.toList << Option.map f
    
    let poll = toMedia pollToMedia tweet.Poll
    let card = toMedia urlCardToMedia tweet.UrlCard
    let gif = toMedia gifAltTextToMedia tweet.GifAltText
    let video = toMedia videoAttributionToMedia tweet.VideoAttribution
    
    let images = tweet.ImageAltTexts |> Array.map (Image << tryNonBlankString) |> Array.toList
        
    MockTweet(
        tweet.Text,
        tweet.Author.ScreenName,
        tweet.Author.Name,
        DateTime.Parse(tweet.DateCreated),
        tweet.Author.Verified,
        tweet.Author.Protected,
        tweet.Retweeter,
        Array.toList tweet.RepliedTo,
        images @ video @ gif @ poll @ card
    )

open Matchers

let fetchTweet filename = 
    let directory = $"{Environment.CurrentDirectory}/../../../tweets/"
    TestTweet.Load(directory + filename)

let speakText (mockTweet:MockTweet) = mockTweet.ToSpeakText()

let fetchSpeakText = fetchTweet >> toMockTweet >> speakText

let inline noTest () = failwith<unit> "Test has not been implemented as of yet"

type ``test json schema is valid``() =
    let template = SchemaTemplate.GetSample()
    let schema = TestTweetSchema.GetSample().JsonValue

    [<Fact>]
    member __.``schema is valid``() =
        schema |> should matchTemplate template

type ``test tweets are valid examples``() =
    
    [<Theory>]
    [<InlineData("emojis/seeNoEvilMonkey.json")>]
    [<InlineData("emojis/faceScreamingInFear.json")>]
    [<InlineData("emojis/fire.json")>]
    [<InlineData("emojis/smilies.json")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json")>]
    [<InlineData("emojis/grimacingFace.json")>]
    [<InlineData("emojis/starstruckRocket.json")>]
    [<InlineData("emojis/rollingOnTheFloorLaughing.json")>]
    [<InlineData("emojis/huggingFace.json")>]
    
    [<InlineData("numbers/negativeNumber.json")>]
    [<InlineData("numbers/phoneNumberOnePlus.json")>]
    [<InlineData("numbers/decimalPercentage.json")>]
    [<InlineData("numbers/numberWithComma.json")>]
    [<InlineData("numbers/temperatures.json")>]
    [<InlineData("numbers/height.json")>]
    [<InlineData("numbers/phoneNumberDots.json")>]
    [<InlineData("numbers/second.json")>]
    [<InlineData("numbers/year.json")>]
    [<InlineData("numbers/centimeters.json")>]
    [<InlineData("numbers/first.json")>]
    [<InlineData("numbers/feet.json")>]

    [<InlineData("numbers/third.json")>]
    [<InlineData("numbers/zeroth.json")>]

    [<InlineData("numbers/dates/mddyy.json")>]
    [<InlineData("numbers/dates/jan6.json")>]
    
    [<InlineData("punctuation/hashtag.json")>]
    [<InlineData("punctuation/percent.json")>]
    [<InlineData("punctuation/underscore.json")>]
    [<InlineData("punctuation/atSymbol.json")>]
    [<InlineData("punctuation/mathEquation.json")>]

    [<InlineData("retweet.json")>]
    [<InlineData("basicPrivateTweet.json")>]
    [<InlineData("basicReply.json")>]
    [<InlineData("basicVerifiedTweet.json")>]
    [<InlineData("gifNoAltText.json")>]
    [<InlineData("imagesAltText.json")>]
    [<InlineData("imageTweetNoAltText.json")>]
    [<InlineData("poll.json")>]
    [<InlineData("quotedTweet.json")>]
    [<InlineData("unverifiedTweet.json")>]
    [<InlineData("urlCard.json")>]
    [<InlineData("videoAttribution.json")>]
    [<InlineData("multipleTcoLinks.json")>]
    [<InlineData("noCardUrlDisplay.json")>]
    [<InlineData("twoReplyingTo.json")>]
    [<InlineData("threeReplyingTo.json")>]
    [<InlineData("fourReplyingTo.json")>]
    [<InlineData("sevenReplyingTo.json")>]

    member __.``examples are valid``(relativeFilepath:string) =
        let testTweet = fetchTweet(relativeFilepath)
        let schema = TestTweetSchema.GetSample()
        testTweet.JsonValue |> should matchSchema schema

type ``tweet times are properly shown``() =

    [<Fact>]
    member __.``tweet times are correctly displayed``() =
        noTest ()

type ``verified tweets are properly parsed``() =

    [<Theory>]
    [<InlineData("basicVerifiedTweet.json")>]
    member __.``speak text should indicate that the author is verified for verified accounts``(relativePath:string) =
        let speakText = fetchSpeakText relativePath
        speakText |> should haveSubstring " verified account "

    [<Theory>]
    [<InlineData("unverifiedTweet.json")>]
    member __.``speak test should NOT indicate that the author is verified for unverified accounts``(relativePath:string) =
        let speakText = fetchSpeakText relativePath
        speakText |> should not' (haveSubstring " verified account ")

type ``tweets from protected accounts are properly parsed``() =
    let testTweet = fetchTweet("basicPrivateTweet.json")
    
    [<Theory>]
    [<InlineData("basicPrivateTweet.json")>]
    member __.``speak text should indicate that the author is private for private accounts``(relativePath:string) =
        let speakText = fetchSpeakText relativePath
        speakText |> should haveSubstring " protected account "

    [<Theory>]
    [<InlineData("basicVerifiedTweet.json")>]
    member __.``speak text should NOT indicate that that author is private for public accounts``(relativePath:string) =
        let speakText = fetchSpeakText relativePath
        speakText |> should not' (haveSubstring " protected account ")

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
                mockTweet.Media
            )
        let speakText = newMockTweet.ToSpeakText()
        speakText |> should haveSubstring expected

    [<Fact>]
    member __.``Finished polls should display the options and percentages of votes``() =
        noTest ()

    [<Fact>]
    member __.``Polls should output the list of options``() =
        noTest ()

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
                mockTweet.Media
            )
        let speakText = newMockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring "final results")

type ``image tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("imageTweetNoAltText.json")>]
    member __.``images without alt text output the word "image"``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring " image "

    [<Theory>]
    [<InlineData("imagesAltText.json")>]
    member __.``images with alt text show the alt text``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        Seq.iter (fun altText -> speakText |> should haveSubstring (processSpeakText altText)) testTweet.Tweet.ImageAltTexts

type ``video tweets are properly parsed`` () =

    [<Theory>]
    [<InlineData("videoAttribution.json")>]
    member __.``videos with attribution display that attribution``(filepath:string) =
        let testTweet = fetchTweet filepath
        let speakText = (toMockTweet testTweet).ToSpeakText()
        speakText |> should haveSubstring (Option.get testTweet.Tweet.VideoAttribution)

type ``gif tweets are properly parsed``() =
    
    [<Theory>]
    [<InlineData("gifNoAltText.json")>]
    member __.``GIFs without alt text output the words "animated image"``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring "animated image"

    [<Fact>]
    member __.``GIFs with alt text show the alt text``() =
        noTest ()

type ``replies are properly parsed``() =
    
    [<Theory>]
    [<InlineData("basicReply.json")>]
    [<InlineData("twoReplyingTo.json")>]
    [<InlineData("threeReplyingTo.json")>]
    [<InlineData("fourReplyingTo.json")>]
    member __.``replies properly show the screen names of the accounts being replied to``(filepath:string) =
        noTest ()

type ``retweets are properly parsed``() =

    [<Fact>]
    member __.``retweets properly show the name of the account retweeting the tweet``() =
        noTest ()

type ``quoted tweets are properly parsed``() =
    
    [<Fact>]
    member __.``quoted tweets should be shown``() =
        noTest ()

    [<Theory>]
    [<InlineData("quotedTweet.json")>]
    member __.``there should be an indication that a tweet is being quoted``(filepath:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring "quote tweet"

    [<Fact>]
    member __.``quoted tweets with polls in the quote tweet should be indicated``() =
        noTest ()

type ``numbers are properly converted to words``() =
    
    [<Theory>]
    [<InlineData("numbers/phoneNumberOnePlus.json", "+1 912-612-4665", "nine one two. six one two. four six six five")>]
    member __.``phone numbers are converted to words properly``(filepath:string, number:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (number, expected)

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
    [<InlineData("numbers/numberWithComma.json", "1,100", "one thousand one hundred")>]
    member __.``whole numbers are converted to word form``(filepath: string, number:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (number, expected)

    [<Theory>]
    [<InlineData("numbers/second.json", "2nd", "second")>]
    [<InlineData("numbers/third.json", "3rd", "third")>]
    [<InlineData("numbers/first.json", "1st", "first")>]
    [<InlineData("numbers/zeroth.json", "0th", "zeroth")>]
    member __.``ordinal numbers (e.g. 2nd) are converted to word form``(filepath:string, ordinal:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (ordinal, expected)

    [<Theory>]
    [<InlineData("numbers/year.json", "2008", "two thousand eight")>]
    member __.``obvious years are properly converted to words``(filepath:string, year:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (year, expected)

    [<Fact>]
    member __.``number ranges are converted to words``() =
        noTest ()

    [<Fact>]
    member __.``times are converted to words``() =
        noTest ()

    [<Theory>]
    [<InlineData("numbers/dates/jan6.json", "Jan. 6", "january sixth")>]
    member __.``obvious dates are converted into words``(filepath:string, date:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (date, expected)

    [<Fact>]
    member __.``date ranges are converted into words``() =
        noTest ()

    [<Theory>]
    [<InlineData("numbers/centimeters.json", "5 cm", "five centimeters")>]
    [<InlineData("numbers/feet.json", "6 ft", "six feet")>]
    member __.``units of measurement (e.g. cm, ft, yds) are converted to words``(filepath:string, measurement:string, expected:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstitution (measurement, expected)

type ``emojis are properly converted to words``() =

    //TODO: change to substitution test with the original emoji
    [<Theory>]
    [<InlineData("emojis/fire.json", "fire")>]
    [<InlineData("emojis/smilies.json", "smiling face with smiling eyes")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json", "skull")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json", "loudly crying face")>]
    [<InlineData("emojis/faceScreamingInFear.json", "face screaming in fear")>]
    [<InlineData("emojis/seeNoEvilMonkey.json", "see no evil monkey")>]
    [<InlineData("emojis/starstruckRocket.json", "star struck")>]
    [<InlineData("emojis/starstruckRocket.json", "rocket")>]
    [<InlineData("emojis/rollingOnTheFloorLaughing.json", "rolling on the floor laughing")>]
    [<InlineData("emojis/grimacingFace.json", "grimacing face")>]
    [<InlineData("emojis/huggingFace.json", "hugging face")>]
    member __.``Emojis should have correct speak text``(filepath:string, name:string) =
        let speakText = fetchSpeakText filepath
        speakText |> should haveSubstring name
        

type ``currency is properly converted to words``() =    
    [<Fact>]
    member __.``Dollar amounts are properly indicated``() =
       noTest ()

    [<Fact>]
    member __.``Euro amounts are properly indicated``() =
        noTest ()

    [<Fact>]
    member __.``British pound amounts are properly indicated``() =
        noTest ()

    [<Fact>]
    member __.``Japanese yen amounts are properly indicated``() =
        noTest ()
        
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

    [<Theory>]
    [<InlineData("basicReply.json")>]
    member __.``beginning replies are removed from the tweet text``(filepath:string) =
        noTest ()
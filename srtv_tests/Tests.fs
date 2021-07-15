module Tests

open System

open Xunit
open FsUnit.Xunit
open SRTV.TweetMedia
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

let matchSchema (schema:TestTweetSchema.Root) = SchemaMatcher.matchSchema(schema.JsonValue)
let matchTemplate (template:SchemaTemplate.Root) = SchemaMatcher.matchSchema(template.JsonValue)
let inline matchPattern (pattern:string) (input:string) = Regex.IsMatch(input, pattern) |> equal

let pollToMedia (poll:TestTweet.Poll) =
    let options = 
        poll.Options
        |> Array.map (fun opt -> (opt.Option, int opt.Votes))
        |> Array.toList
    Poll ( options, DateTime.Parse (poll.EndDate) )

let urlCardToMedia (card:TestTweet.UrlCard) =
    Card (card.Title, card.Description, card.Url)

let gifAltTextToMedia = wrapStringIfNotBlank >> Gif
let videoAttributionToMedia = wrapStringIfNotBlank >> Video

let toMockTweet(root:TestTweet.Root) =
    let tweet = root.Tweet
    let toMedia (f:'a -> Media) = Option.toList << Option.map f
    
    let poll = toMedia pollToMedia tweet.Poll
    let card = toMedia urlCardToMedia tweet.UrlCard
    let gif = toMedia gifAltTextToMedia tweet.GifAltText
    let video = toMedia videoAttributionToMedia tweet.VideoAttribution

    let images = tweet.ImageAltTexts |> Array.map (wrapStringIfNotBlank >> Image) |> Array.toList
        
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

let fetchTweet filename = 
    let directory = $"{Environment.CurrentDirectory}/../../../tweets/"
    TestTweet.Load(directory + filename)

let inline noTest () = failwith<unit> "Test has not been implement as of yet"

type ``test json schema is valid``() =
    let template = SchemaTemplate.GetSample()
    let schema = TestTweetSchema.GetSample().JsonValue

    [<Fact>]
    member __.``schema is valid``() =
        schema |> should matchTemplate template

type ``test tweets are valid examples``() =
    
    [<Theory>]
    [<InlineData("emojis/faceScreamingInFear.json")>]
    [<InlineData("emojis/fire.json")>]
    [<InlineData("emojis/smilies.json")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json")>]
    
    [<InlineData("numbers/negativeNumber.json")>]
    [<InlineData("numbers/phoneNumber.json")>]
    [<InlineData("numbers/decimalPercentage.json")>]
    
    [<InlineData("punctuation/hashtag.json")>]
    [<InlineData("punctuation/percent.json")>]
    [<InlineData("punctuation/underscore.json")>]
    [<InlineData("punctuation/atSymbol.json")>]

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

    member __.``examples are valid``(relativeFilepath:string) =
        let testTweet = fetchTweet(relativeFilepath)
        let schema = TestTweetSchema.GetSample()
        testTweet.JsonValue |> should matchSchema schema

type ``verified tweets are properly parsed``() =

    [<Theory>]
    [<InlineData("basicVerifiedTweet.json")>]
    member __.``speak text should indicate that the author is verified for verified accounts``(relativePath:string) =
        let mockTweet = toMockTweet (fetchTweet relativePath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " verified account "

    [<Theory>]
    [<InlineData("unverifiedTweet.json")>]
    member __.``speak test should NOT indicate that the author is verified for unverified accounts``(relativePath:string) =
        let mockTweet = toMockTweet (fetchTweet relativePath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring " verified account ")

type ``tweets from protected accounts are properly parsed``() =
    let testTweet = fetchTweet("basicPrivateTweet.json")
    
    [<Theory>]
    [<InlineData("basicPrivateTweet.json")>]
    member __.``speak text should indicate that the author is private for private accounts``(relativePath:string) =
        let mockTweet = toMockTweet (fetchTweet relativePath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " protected account "

    [<Theory>]
    [<InlineData("basicVerifiedTweet.json")>]
    member __.``speak text should NOT indicate that that author is private for public accounts``(relativePath:string) =
        let mockTweet = toMockTweet (fetchTweet relativePath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring " protected account ")

type ``poll tweets are properly parsed``() =

    [<Fact>]
    member __.``Finished polls should indicate that they are finished``() =
        noTest ()

    [<Fact>]
    member __.``Unfinished polls should indicate the time they have left``(filepath:string, expected:string) =
       noTest ()

    [<Fact>]
    member __.``Polls should display the options and number of tweets``() =
        noTest ()

type ``image tweets are properly parsed``() =
    
    [<Fact>]
    member __.``images without alt text output the word "image"``() =
        noTest ()

    [<Fact>]
    member __.``images with alt text show the alt text``() =
        noTest ()


type ``video tweets are properly parsed`` () =

    [<Fact>]
    member __.``videos with attribution display that attribution``() =
        noTest ()

type  ``gif tweets are properly parsed``() =
    
    [<Fact>]
    member __.``GIFs without alt text output the words "animated image"``() =
        noTest ()

    [<Fact>]
    member __.``GIFs with alt text show the alt text``() =
        noTest ()

type ``replies are properly parsed``() =
    
    [<Fact>]
    member __.``replies properly show the screen names of the accounts being replied to``() =
        noTest ()

type ``retweets are properly parsed``() =

    [<Fact>]
    member __.``retweets properly show the name of the account retweeting the tweet``() =
        noTest ()

type ``quoted tweets are properly parsed``() =
    
    [<Fact>]
    member __.``quoted tweets should be shown``() =
        noTest ()

    [<Fact>]
    member __.``quoted tweets with polls in the quote tweet should be indicated``() =
        noTest ()

type ``numbers are properly converted to words``() =

    [<Theory>]
    [<InlineData("numbers/negativeNumber.json")>]
    member __.``numbers under zero include "minus"``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring (" minus ")
        speakText |> should not' (haveSubstring "-")

    [<Theory>]
    [<InlineData("numbers/decimalPercentage.json", "67.94", "sixty seven point nine four")>]
    [<InlineData("numbers/decimalPercentage.json", "58.41", "fifty eight point four one")>]
    member __.``decimal numbers (e.g. 3.45) are converted to the form "three point four five"``(filepath:string, decimal:string, expected:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring expected
        speakText |> should not' (haveSubstring decimal)

    [<Fact>]
    member __.``whole numbers are converted to word form``() =
        noTest ()

    [<Fact>]
    member __.``ordinal numbers (e.g. 2nd) are converted to word form``() =
        noTest ()

    [<Fact>]
    member __.``obvious years are properly converted to words``() =
        noTest ()

    [<Fact>]
    member __.``number ranges are converted to words``() =
        noTest ()

    [<Fact>]
    member __.``times are converted to words``() =
        noTest ()

    [<Fact>]
    member __.``obvious dates are converted into words``() =
        noTest ()

    [<Fact>]
    member __.``abbreviated numbers (e.g. 50K, 100M) are converted to words``() =
        noTest ()

type ``emojis are properly converted to words``() =

    [<Theory>]
    [<InlineData("emojis/fire.json", " fire ")>]
    [<InlineData("emojis/smilies.json", " smiling face with smiling eyes ")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json", " skull ")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json", " loudly crying face ")>]
    [<InlineData("emojis/faceScreamingInFear.json", " face screaming in fear ")>]
    member __.``Emojis should have correct speak text``(filepath:string, name:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
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
        
type ``punctuation is properly converted to words``() = 

    [<Theory>]
    [<InlineData("punctuation/hashtag.json")>]
    member __.``# is replaced with the word "hashtag"``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " hashtag "
        speakText |> should not' (haveSubstring "#")

    [<Theory>]
    [<InlineData("punctuation/underscore.json")>]
    member __.``Underscores are replaced with the word "underscore"``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " underscore "
        speakText |> should not' (haveSubstring "_")

    [<Theory>]
    [<InlineData("punctuation/percent.json")>]
    member __.``"%" is replaced with the word "percent"``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " percent "
        speakText |> should not' (haveSubstring "%")

    [<Theory>]
    [<InlineData("urlCard.json")>]
    [<InlineData("imagesAltText.json")>]
    [<InlineData("multipleTcoLinks.json")>]
    member __.``t.co links are removed from the tweet``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring "t.co/")

    [<Theory>]
    [<InlineData("punctuation/atSymbol.json")>]
    member __.``at symbol is replaced with the word "at" ``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should not' (haveSubstring "@")

    [<Fact>]
    member __.``degree symbol is replaced with the word "degree"``() =
        noTest ()

    [<Fact>]
    member __.``parens do not appear in the tweet``() =
        noTest ()

    [<Fact>]
    member __.``dashes/hyphens do not appear in tweets``() =
        noTest ()

    [<Theory>]
    [<InlineData("basicReply.json")>]
    member __.``beginning replies are removed from the tweet``(filepath:string) =
        noTest ()
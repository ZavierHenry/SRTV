module Tests

open System

open Xunit
open FsUnit.Xunit
open SRTV.TweetMedia
open FSharp.Data

open NHamcrest
open NHamcrest.Core

open Newtonsoft.Json.Schema
open Newtonsoft.Json.Linq

open System.Collections.Generic

let [<Literal>] samplesFile = "./samples.json"
let [<Literal>] schemaFile = "./schema.json"
let [<Literal>] templateFile = "http://json-schema.org/draft-04/schema"

type TestTweet = JsonProvider<samplesFile, SampleIsList=true, InferTypesFromValues=false>
type SchemaTemplate = JsonProvider<templateFile>
type TestTweetSchema = JsonProvider<schemaFile>

type SchemaMatcher(sample:string) = 
    inherit DiagnosingMatcher<obj>()

    let schema = JSchema.Parse <| sample
    let mutable messages = List() :> IList<string>

    override this.DescribeTo(description) =
        description.AppendText("JSON should validate against this schema") |> ignore

    override this.Matches(item, description) =
        let token = item :?> JsonValue
        let token = JToken.Parse <| token.ToString()
        let result = token.IsValid(schema, &messages)

        if not result then this.DescribeMismatch(item, description)
        result

    override this.DescribeMismatch(item, mismatchDescription) =
        Seq.iter (fun err -> mismatchDescription.AppendText(err + "\n\n") |> ignore) messages

    interface IMatcher<obj> with
        member this.Matches(item:obj) =
            this.Matches(item, StringDescription())

        member this.DescribeMismatch(item, mismatchDescription) =
            Seq.iter (fun err -> mismatchDescription.AppendText(err + "\n\n") |> ignore) messages

    static member matchSchema(value:JsonValue) = SchemaMatcher <| value.ToString()


let matchSchema (schema:TestTweetSchema.Root) = SchemaMatcher.matchSchema(schema.JsonValue)
let matchTemplate (template:SchemaTemplate.Root) = SchemaMatcher.matchSchema(template.JsonValue)
    
let pollToMedia (poll:TestTweet.Poll) =
    let endDate = DateTime.Parse(poll.EndDate)
    let options = 
        poll.Options
        |> Array.map (fun opt -> (opt.Option, int opt.Votes))
        |> Array.toList
    Poll (options, endDate)

let urlCardToMedia (card:TestTweet.UrlCard) =
    Card (card.Title, card.Description, card.Url)

let gifAltTextToMedia = wrapStringIfNotBlank >> Gif
let videoAttributionToMedia = wrapStringIfNotBlank >> Video

let toMockTweet(root:TestTweet.Root) =
    let tweet = root.Tweet
    let poll = 
        tweet.Poll
        |> Option.map (pollToMedia >> List.singleton)
        |> Option.defaultValue []

    let card =
        tweet.UrlCard
        |> Option.map (urlCardToMedia >> List.singleton) 
        |> Option.defaultValue []

    let gif = 
        tweet.GifAltText
        |> Option.map (gifAltTextToMedia >> List.singleton)
        |> Option.defaultValue []

    let video =
        tweet.VideoAttribution
        |> Option.map (videoAttributionToMedia >> List.singleton)
        |> Option.defaultValue []

    let images =
        tweet.ImageAltTexts
        |> Array.map (wrapStringIfNotBlank >> Image)
        |> Array.toList
        
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

type ``test json schema is valid``() =
    let template = SchemaTemplate.GetSample()
    let schema = TestTweetSchema.GetSample().JsonValue

    [<Fact>]
    member __.``schema is valid``() =
        schema |> should matchTemplate template

type ``test tweets are valid examples``() =
    
    [<Theory>]
    [<InlineData("emojis/fire.json")>]
    [<InlineData("emojis/smilies.json")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json")>]
    
    [<InlineData("numbers/phoneNumber.json")>]
    
    [<InlineData("punctuation/hashtag.json")>]
    [<InlineData("punctuation/percent.json")>]

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
        raise <| System.NotImplementedException("Test has not been implemented")


type ``numbers are properly converted to words``() =

    [<Fact>]
    member __.``numbers under zero include "negative"``() =
       raise <| System.NotImplementedException("Test has not been implemented")

type ``emojis are properly converted to words``() =

    [<Theory>]
    [<InlineData("emojis/fire.json", " fire ")>]
    [<InlineData("emojis/smilies.json", " smiling face with smiling eyes ")>]
    [<InlineData("emojis/loudlyCryingWithSkull.json", " skull ")>]
    member __.``Emojis should have correct speak text``(filepath:string, name:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring name
        

type ``currency is properly converted to words``() =    
    [<Fact>]
    member __.``Dollar amounts are properly indicated``() =
        raise <| System.NotImplementedException("Test has not been implemented")
        
type ``punctuation is properly converted to words``() = 

    [<Theory>]
    [<InlineData("punctuation/hashtag.json")>]
    member __.``# is replaced with the word "hashtag"``(filepath:string) =
        let mockTweet = toMockTweet (fetchTweet filepath)
        let speakText = mockTweet.ToSpeakText()
        speakText |> should haveSubstring " hashtag "

    [<Fact>]
    member __.``Underscores are replaced with the word "underscore"``() =
        raise <| System.NotImplementedException("Test has not been implemented")

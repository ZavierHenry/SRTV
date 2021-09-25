// Learn more about F# at http://fsharp.org

open System
open System.IO

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

open SRTV.Twitter.TwitterClient
open SRTV.Twitter.Patterns
open SRTV.Twitter.Patterns.Mentions
open SRTV.SRTVResponse

open SRTV.Utilities

open System.Reflection

let exampleMockTweet =
    MockTweet(
        "I love that the Harvey’s burger chain got its name because the John Harvey Motors car dealership was going out of business and a guy opening a burger shop got the dealership sign for cheap.  A thrifty Canadian icon.",
        "anne_theriault",
        "Anne Thériault",
        DateTime.Parse("2021-05-07T16:31:38.000Z").ToUniversalTime(),
        true,
        false,
        None,
        [],
        None,
        [],
        []
)

let synthesize text outfile =
    Synthesizer().Synthesize(text, outfile)

let speak text filename =
    Synthesizer().Speak(text, filename)


let toImage'(output:string) =
    let source = "Twitter for iPhone"
    let profileUrl = "https://pbs.twimg.com/profile_images/1011409104441630720/ksmEpPII_normal.jpg"

    async {
        let! bytes = toImage exampleMockTweet profileUrl source DateTime.UtcNow <| Image (Theme.Dim, Version.Regular)
        return File.WriteAllBytes(output, bytes)
    }

type RenderRequest = 
    {
        requestTweetID: string
        requestDateTime: DateTime
        renderTweetID: string
        renderOptions: RenderOptions
    }
    static member init (requestTweetID: string, requestDateTime: DateTime, renderTweetID: string, renderOptions: RenderOptions) =
        {
            requestTweetID = requestTweetID
            requestDateTime = requestDateTime
            renderTweetID = renderTweetID
            renderOptions = renderOptions
        }


let rec handleMentions (client:Client) startDate (token: string option) = async {

    let! mentions = client.GetMentions(startDate)
    let requests = 
        let toVersion fullVersion = if fullVersion then Version.Full else Version.Regular
        mentions
        |> ClientResult.map (fun mentions -> mentions.Tweets)
        |> ClientResult.map (Seq.filter (function
            | VideoRenderMention _ 
            | ImageRenderMention _ 
            | TextRenderMention _ 
            | GeneralRenderMention _ -> true
            | _ -> false ))
        |> ClientResult.map (Seq.map (function
            | VideoRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Video (toVersion fullVersion))
            | ImageRenderMention (requestTweetID, requestDateTime, theme, fullVersion, renderTweetID) -> 
                RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Image (Theme.fromAttributeValue theme |> Option.defaultValue Theme.Dim, toVersion fullVersion))
            | TextRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Text (toVersion fullVersion))
            | GeneralRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Video (toVersion fullVersion))
            | _ -> RenderRequest.init ("", DateTime.UtcNow, "", Video Version.Regular)))

    let! tweets =
        requests
        |> ClientResult.map ( Seq.map (fun {renderTweetID = id} -> id) )
        |> ClientResult.bindAsync client.GetTweets

        
    //TODO: convert to SRTV tweet and send

    match mentions with
    | Success mentions ->
        match mentions.Meta with
        | null -> return ()
        | meta ->
            match meta.NextToken with
            | "" | null -> return ()
            | token -> handleMentions client startDate (Some token) |> Async.Start
    | TwitterError (message, exn) ->
        printfn "A Twitter error has occurred: %s" message
        printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
    | OtherError (message, exn) ->
        printfn "An error has occurred: %s" message
        printfn "Error: %O, Stack trace: %s" exn exn.StackTrace

}

let isDevelopmentEnvironment () =
    let environmentVariable = Environment.GetEnvironmentVariable("NETCORE_ENVIRONMENT")
    String.IsNullOrEmpty(environmentVariable) || environmentVariable.ToLower() = "development"

let sendTweet text (client:Client) =
    let tweet = TextTweet text
    async {
        match! client.TweetAsync tweet with
        | Success _ -> printfn "Sending tweet was successful..."
        | TwitterError (message, exn) ->
            printfn "Twitter error message: %s" message
            printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        | OtherError (message, exn) ->
            printfn "Non-Twitter error message: %s" message
            printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
    }

    
let getTweet id (client:Client) =
    async {
        match! Seq.singleton id |> client.GetTweets with
        | Success tweetQuery ->
            printfn "Querying tweets was successful"
            printfn "Tweet ID is %s" tweetQuery.Tweets.[0].ID
            printfn "Tweet alt text is %s" tweetQuery.Includes.Media.[0].AltText
        | TwitterError (message, exn) ->
            printfn "Twitter error message: %s" message
            printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        | OtherError (message, exn) ->
            printfn "Non-Twitter error message: %s" message
            printfn "Error: %O, Stack trace %s" exn exn.StackTrace
    }
   
let buildClient () =
    let builder = ConfigurationBuilder()
    Some ()
    |> Option.filter (fun _ -> isDevelopmentEnvironment())
    |> Option.map (fun _ -> AppDomain.CurrentDomain.FriendlyName |> AssemblyName |> Assembly.Load)
    |> Option.filter (function | null -> false | _ -> true)
    |> Option.map (fun assembly -> builder.AddUserSecrets(assembly, true, true).Build() |> Client)

[<EntryPoint>]
let main argv =

    match argv with
    | [| "synthesize"; text; outfile |] -> synthesize text outfile
    | [| "synthesize"; text |] -> synthesize text <| Path.Join (Environment.CurrentDirectory, "synthesis.mp4")
    | [| "speak"; text; outfile |] -> speak text outfile
    | [| "speak"; text |] -> speak text <| Path.Join (Environment.CurrentDirectory, "speakText.wav")
    | [| "image"; outfile |] -> toImage' outfile
    | [| "image" |] -> toImage' <| Path.Join (Environment.CurrentDirectory, "sampleImage.jpg")
    | [| "sendTweet"; text |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> sendTweet text client
    | [| "getTweet"; id |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> getTweet id client
    | ps -> async { printfn "Program cannot understand parameters: %s" <| String.concat " | " ps }
    |> Async.RunSynchronously
    printfn "End of program..."
    0

    

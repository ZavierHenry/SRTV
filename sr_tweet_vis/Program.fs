// Learn more about F# at http://fsharp.org

open System
open System.IO

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting

open FSharp.Data

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

open SRTV.Twitter
open SRTV.Twitter.TwitterClient
open SRTV.Twitter.Patterns
open SRTV.Twitter.Patterns.Mentions
open SRTV.SRTVResponse

open SRTV.Utilities

open System.Reflection



[<Literal>]
let private schema = 
    """[
        { "queuedTweets": [ "a" ], "defaultQueryDateTime": "8/30/2021 12:00:00 AM", "rateLimit": { "limited": false } },
        { "queuedTweets": [ "b" ], "defaultQueryDateTime": "8/30/2021 12:00:00 AM", "queryDateTime": "9/13/2021 3:45:00 PM", "rateLimit": { "limited": true, "nextQueryDateTime": "8/30/2021 12:00:00 AM" } }
       ]"""

type AppSettings = JsonProvider<schema, SampleIsList=true>
let buildAppSettings () = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "appsettings.json") |> AppSettings.Load

type AppSettings.Root with
    static member enqueueTweetID tweetID (appsettings:AppSettings.Root)  =
        let queuedTweets = Array.append appsettings.QueuedTweets [| tweetID |]
        AppSettings.Root(queuedTweets, appsettings.DefaultQueryDateTime, appsettings.RateLimit, appsettings.QueryDateTime)
   
    static member dequeueTweetID tweetID (appsettings:AppSettings.Root)  =
        let queuedTweets = Array.except [tweetID] appsettings.QueuedTweets
        AppSettings.Root(queuedTweets, appsettings.DefaultQueryDateTime, appsettings.RateLimit, appsettings.QueryDateTime)

    static member clearRateLimit (appsettings:AppSettings.Root) =
        let rateLimit = AppSettings.RateLimit(false, None)
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultQueryDateTime, rateLimit, appsettings.QueryDateTime)

    static member setRateLimit (currentDateTime:DateTime) (appsettings:AppSettings.Root) =
        let rateLimit = AppSettings.RateLimit(true, Some <| currentDateTime.AddMinutes 5.0)
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultQueryDateTime, rateLimit, appsettings.QueryDateTime)

    static member setQueryDateTime queryDateTime (appsettings:AppSettings.Root)  =
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultQueryDateTime, appsettings.RateLimit, Some queryDateTime)

    static member saveAppSettings (appsettings:AppSettings.Root) =
        File.WriteAllText("appsettings.json", appsettings.ToString())


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

let logClientResultError desc = function
    | Success _ -> ()
    | TwitterError (message, exn) ->
        printfn "Twitter error occurred when %s: %s" desc message
        printfn "Error received from Twitter: %A" exn
    | OtherError (message, exn) ->
        printfn "Non-Twitter error occurred when %s: %s" desc message
        printfn "Exception raised: %A" exn


let render (client:Client) tweet includes map (request:RenderRequest) =
    let mockTweet =
        MockTweet(tweet, includes, Map.tryFind tweet.ID map |> Option.defaultValue Seq.empty)

    let ref = request.requestDateTime

    match request.renderOptions with
    | Video version ->
        async {
            use tempfile = new TempFile()
            try
                do! Synthesizer().Synthesize(mockTweet, tempfile.Path, request.requestDateTime, request.renderOptions)
                let srtvTweet = AudioTweet (tempfile.Path, "Hello! Your video is here and should be attached. Thank you for using the SRTV bot")
                return! client.ReplyAsync(uint64 request.requestTweetID, srtvTweet)
            with exn -> return OtherError ("Video could not be made", exn)
        }
    | Image (_, version) ->
        let text = match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
        let profilePicUrl = tryFindUserById tweet.AuthorID includes |> Option.map (fun user -> user.ProfileImageUrl) |> Option.defaultValue ""
        
        async {
            let! image = toImage mockTweet profilePicUrl tweet.Source ref request.renderOptions
            let srtvTweet = ImageTweet (image, "Hello! Your image is here and should be attached. There should also be alt text in the image. If the alt text is too big for the image, it will be tweeted in the replies. Thank you for using the SRTV bot", text)
            return! client.ReplyAsync(uint64 request.requestTweetID, srtvTweet)
        }

    | Text version ->
        let text = match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
        let srtvTweet = TextTweet $"Hello! Your text is here and should be below. There will be multiple tweets if the text is too many characters. Thank you for using the SRTV bot.\n\n\n%s{text}"
        client.ReplyAsync(uint64 request.requestTweetID, srtvTweet)

let handleTweet client tweet includes map request = async {
    let! result = render client tweet includes map request
    logClientResultError (sprintf "trying to render tweet ID %s" tweet.ID) result
}


let handleError (client:Client) (error:LinqToTwitter.Common.TwitterError) (request:RenderRequest) =
    
    let replyID = uint64 request.requestTweetID

    async {
        let srtvTweet = 
            match error.Title with
            | "Not Found Error" -> 
                TextTweet "Sorry, this tweet cannot be found to be rendered. Perhaps the tweet is deleted or the account is set to private?"
            | "Authorization Error" -> 
                TextTweet "Sorry, there was an authorization error when trying to render this tweet"
            | _ -> 
                TextTweet "Sorry, there was an error from Twitter when trying to render this tweet"

        let! result = client.ReplyAsync(replyID, srtvTweet)
        logClientResultError (sprintf "sending error about tweet %s" error.ID) result
    }
        
let rec handleMentions client appsettings = 
    
    let rec handleMentions' (client:Client) (appsettings:AppSettings.Root) endDate (updatedAppsettings:AppSettings.Root) (token: string option) = async {

        let startDate = appsettings.QueryDateTime |> Option.defaultValue appsettings.DefaultQueryDateTime
    
        let! mentions =
            token
            |> Option.map (fun token -> client.GetMentions(startDate, endDate, token))
            |> Option.defaultWith (fun () -> client.GetMentions(startDate, endDate))

        logClientResultError "getting mentions for the client" mentions

        let requests = 
            let toVersion fullVersion = if fullVersion then Version.Full else Version.Regular
            mentions
            |> ClientResult.map (fun mentions -> 
                nullableSequenceToValue mentions.Tweets
                |> Seq.choose (function
                    | VideoRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                        Some <| RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Video (toVersion fullVersion))
                    | ImageRenderMention (requestTweetID, requestDateTime, theme, fullVersion, renderTweetID) -> 
                        Some <| RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Image (Theme.fromAttributeValue theme |> Option.defaultValue Theme.Dim, toVersion fullVersion)) 
                    | TextRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                        Some <| RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Text (toVersion fullVersion))
                    | GeneralRenderMention (requestTweetID, requestDateTime, fullVersion, renderTweetID) ->
                        Some <| RenderRequest.init (requestTweetID, requestDateTime, renderTweetID, Video (toVersion fullVersion))
                    | _ -> None))

        let! responseQuery =
            requests
            |> ClientResult.map ( Seq.map (fun {renderTweetID = id} -> id) )
            |> ClientResult.bindAsync client.GetTweets

        logClientResultError "getting requested renderable tweets" responseQuery

        let! extendedEntities =
            responseQuery
            |> ClientResult.map (fun resp ->
                resp.Tweets
                |> nullableSequenceToValue
                |> Seq.filter (fun tweet -> 
                    Option.ofObj tweet.Attachments 
                    |> Option.map (fun attachments -> nullableSequenceToValue attachments.MediaKeys)
                    |> Option.exists (not << Seq.isEmpty))
                |> Seq.map (fun tweet -> tweet.ID))
            |> ClientResult.bindAsync client.getTweetMediaEntities

        logClientResultError "getting extended entities for renderable tweets with media" extendedEntities

        let requestHandlers = 
            extendedEntities
            |> ClientResult.map (fun ee -> ee |> Seq.map (fun (key, value) -> (string key, Seq.ofList value) ) |> Map)
            |> ClientResult.bind (fun ee -> responseQuery |> ClientResult.map (fun resp -> (resp, ee)))
            |> ClientResult.bind (fun (resp, ee) -> requests |> ClientResult.map (fun requests -> (requests, resp, ee)))
            |> ClientResult.map (fun (requests, resp, ee) ->
                requests
                |> Seq.map (fun request -> 
                    resp.Tweets
                    |> nullableSequenceToValue
                    |> Seq.tryFind (fun tweet -> request.renderTweetID = tweet.ID)
                    |> Option.map (fun tweet -> handleTweet client tweet resp.Includes ee request)
                    |> Option.orElseWith (fun () -> 
                        resp.Errors
                        |> nullableSequenceToValue
                        |> Seq.tryFind (fun error -> request.renderTweetID = error.ID)
                        |> Option.map (fun error -> handleError client error request))
                    |> Option.defaultWith (fun () -> async { return () } )))

        let! renderResults = 
            match requestHandlers with
            | Success handlers -> Async.Parallel handlers
            | _ -> async { return Array.empty }

        match requestHandlers with
        | Success handlers -> handlers |> Async.Parallel |> Async.Ignore |> Async.Start
        | TwitterError (message, exn) ->
            printfn "Twitter error occurred: %s" message
            printfn "Error received from Twitter: %A" exn
        | OtherError (message, exn) ->
            printfn "Error occurred: %s" message
            printfn "Exception raised: %A" exn

        match mentions with
        | Success mentions ->
            match mentions.Meta with
            | null -> return ()
            | meta ->
                match meta.NextToken with
                | "" | null -> 
                    updatedAppsettings
                    |> AppSettings.Root.clearRateLimit
                    |> AppSettings.Root.setQueryDateTime endDate
                    |> AppSettings.Root.saveAppSettings
                    return ()
                | token -> handleMentions' client appsettings endDate updatedAppsettings (Some token) |> Async.Start
        | TwitterError (message, exn) ->
            match exn.StatusCode with
            | Net.HttpStatusCode.TooManyRequests -> 
                updatedAppsettings
                |> AppSettings.Root.setRateLimit endDate
                |> AppSettings.Root.saveAppSettings
            | _ -> ()
        | OtherError (message, exn) -> return ()

    }
    
    handleMentions' client appsettings DateTime.UtcNow appsettings None

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
    //| [| "mentions" |] -> 
    //    match buildClient() with
    //    | None -> async { printfn "Client cannot be built..." }
    //    | Some client -> handleMentions client
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

    

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
open System.Text.RegularExpressions



[<Literal>]
let private schema = 
    """[
        { "queuedTweets": [ 
                {  
                    "requestTweetID": "a", 
                    "renderType": "a",
                    "text": "a",
                    "renderTweetID": "a"
                } 
            ], "defaultQueryDateTime": "8/30/2021 12:00:00 AM", "rateLimit": { "limited": false } 
        },
        { "queuedTweets": [ 
                { 
                    "requestTweetID": "a", 
                    "renderType": "a",
                    "text": "a",
                    "renderTweetID": "a",
                    "imageInfo": {
                        "source": "a",
                        "profilePicture": "a",
                        "date": "8/30/2021 12:00:00 AM",
                        "verified": true,
                        "protected": true,
                        "name": "a",
                        "screenName": "a"
                    }
                } 
            ], "defaultQueryDateTime": "8/30/2021 12:00:00 AM", "queryDateTime": "9/13/2021 3:45:00 PM", "rateLimit": { "limited": true, "nextQueryDateTime": "8/30/2021 12:00:00 AM" } }
       ]"""

type AppSettings = JsonProvider<schema, SampleIsList=true>
let buildAppSettings () = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "appsettings.json") |> AppSettings.Load

type AppSettings.Root with
    static member enqueueTweet (queuedTweet: AppSettings.QueuedTweet) (appsettings:AppSettings.Root)  =
        let queuedTweets =
            appsettings.QueuedTweets
            |> Array.filter (fun x -> x.RequestTweetId <> queuedTweet.RequestTweetId)
            |> Array.append [| queuedTweet |]
        AppSettings.Root(queuedTweets, appsettings.DefaultQueryDateTime, appsettings.RateLimit, appsettings.QueryDateTime)
   
    static member dequeueTweet tweetID (appsettings:AppSettings.Root)  =
        let queuedTweets = appsettings.QueuedTweets |> Array.filter (fun x -> x.RequestTweetId <> tweetID)
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

type RenderType =
    | VideoRender
    | ImageRender of Theme
    | TextRender
    | ErrorRender

    static member serialize = function
        | VideoRender -> "video"
        | ImageRender theme -> $"{Theme.toAttributeValue theme} image"
        | TextRender -> "text"
        | ErrorRender -> "error"

    static member deserialize = function
        | "video" -> Some VideoRender
        | "text" -> Some TextRender
        | "error" -> Some ErrorRender
        | renderType when Regex.IsMatch(renderType, $"^\w+ image$") ->
            let theme = Regex.Match(renderType, $"^(\w+) image$").Groups.[1].Value
            Theme.fromAttributeValue theme |> Option.map ImageRender
        | _ -> None

    static member fromRenderOption = function
        | Video _ -> VideoRender
        | Image (theme, _) -> ImageRender theme
        | Text _ -> TextRender


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
        

let private renderVideo (client:Client) text (tweetID:string) = async {
    use tempfile = new TempFile()
    try
        do! Synthesizer().Synthesize(text, tempfile.Path)
        let srtvTweet = AudioTweet (tempfile.Path, "Hello! Your video is here and should be attached. Thank you for using the SRTV bot")
        return! client.ReplyAsync(uint64 tweetID, srtvTweet)
    with exn -> return OtherError("Video could not be made", exn)
}

let private renderImage (client:Client) info theme (tweetID: string) = async {
    let! image = tweetInfoToImage info theme
    let srtvTweet = ImageTweet (image, "Hello! Your image is here and should be attached. There should also be alt text in the image. If the alt text is too big for the image, it will be tweeted in the replies. Thank you for using the SRTV bot", info.text)
    return! client.ReplyAsync(uint64 tweetID, srtvTweet)
}

let private renderText (client:Client) text (tweetID: string) = client.ReplyAsync(uint64 tweetID, TextTweet text)

let render (client:Client) tweet includes map (request:RenderRequest) =
    let mockTweet =
        MockTweet(tweet, includes, Map.tryFind tweet.ID map |> Option.defaultValue Seq.empty)

    let ref = request.requestDateTime

    match request.renderOptions with
    | Video version ->
        let text = match version with | Version.Full -> mockTweet.ToSpeakText(ref) | Version.Regular -> mockTweet.ToFullSpeakText(ref)
        renderVideo client text request.requestTweetID
    | Image (theme, version) ->
        let text = match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
        let info = {
            name = mockTweet.Name
            screenName = mockTweet.ScreenName
            profileUrl = 
                tryFindUserById tweet.AuthorID includes 
                |> Option.map (fun user -> user.ProfileImageUrl |> Option.ofObj |> Option.defaultValue "") 
                |> Option.defaultValue ""
            locked = mockTweet.IsProtected
            verified = mockTweet.IsVerified

            text = text
            source = tweet.Source |> Option.ofObj |> Option.defaultValue ""
            date = mockTweet.Date
        }
        renderImage client info theme request.requestTweetID

    | Text version ->
        let text = 
            match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
            |> sprintf "Hello! Your text is here and should be below. There will be multiple tweets if the text is too many characters. Thank you for using the SRTV bot.\n\n\n%s"
        renderText client text request.requestTweetID



let rec handleException (client:Client) (renderTweetID: string) (requestTweetID: string) = function
    | Success value -> async { return Success value }
    | OtherError (message, exn) ->
        async {
            let text = "Sorry, there was an error when trying to render your tweet"
            let! result = client.ReplyAsync(uint64 requestTweetID, TextTweet text)
            logClientResultError (sprintf "when trying to render tweet ID %s from tweet ID %s" renderTweetID requestTweetID) result

            return 
                match result with
                | Success () -> Success <| Choice1Of2 requestTweetID
                | TwitterError (message, exn) ->
                    match exn.StatusCode with
                    | Net.HttpStatusCode.TooManyRequests 
                    | Net.HttpStatusCode.ServiceUnavailable
                    | Net.HttpStatusCode.InternalServerError 
                    | Net.HttpStatusCode.BadGateway ->
                        AppSettings.QueuedTweet(requestTweetID, RenderType.serialize TextRender, text, renderTweetID, None)
                        |> Choice2Of2
                        |> Success
                    | _ -> TwitterError (message, exn)
                | OtherError (message, exn) -> OtherError (message, exn)
        }
    | TwitterError (message, exn) -> handleException client renderTweetID requestTweetID <| OtherError (message, exn)


let handleQueuedTweet (client:Client) (queuedTweet:AppSettings.QueuedTweet) = async {

    let! result = 
        match RenderType.deserialize queuedTweet.RenderType with
        | Some VideoRender -> renderVideo client queuedTweet.Text queuedTweet.RequestTweetId
        | Some (ImageRender theme) ->
            queuedTweet.ImageInfo
            |> Option.map (fun info ->
                {
                    name = info.Name
                    screenName = info.ScreenName
                    profileUrl = info.ProfilePicture
                    locked = info.Protected
                    verified = info.Verified

                    source = info.Source
                    date = info.Date
                    text = queuedTweet.Text
                })
            |> Option.map (fun info -> renderImage client info theme queuedTweet.RequestTweetId)
            |> Option.defaultWith (fun () -> async { return OtherError ("Queued tweet info was not available", Failure("Queued tweet info was not available"))})

        | Some TextRender | Some ErrorRender -> renderText client queuedTweet.Text queuedTweet.RequestTweetId
        | None -> async { return OtherError("Error sending tweet", Failure($"Could not deserialize render type {queuedTweet.RenderType}")) }

    logClientResultError (sprintf "trying to render tweet ID %s" queuedTweet.RenderTweetId) result

    return!
        match result with
        | Success () -> Success <| Choice1Of2 queuedTweet.RequestTweetId
        | TwitterError (message, exn) ->
            match exn.StatusCode with
            | Net.HttpStatusCode.TooManyRequests 
            | Net.HttpStatusCode.InternalServerError
            | Net.HttpStatusCode.InternalServerError 
            | Net.HttpStatusCode.BadGateway -> Success <| Choice2Of2 queuedTweet
            | _ -> TwitterError (message, exn)
        | OtherError (message, exn) -> OtherError (message, exn)
        |> handleException client queuedTweet.RenderTweetId queuedTweet.RequestTweetId
}

let handleTweet client tweet includes map request = async {
    let! result = render client tweet includes map request
    logClientResultError (sprintf "trying to render tweet ID %s" tweet.ID) result

    return! 
        match result with
        | Success () -> Success <| Choice1Of2 request.requestTweetID
        | TwitterError (message, exn) ->
            match exn.StatusCode with
            | Net.HttpStatusCode.TooManyRequests ->
                let mockTweet = MockTweet(tweet, includes, Map.tryFind tweet.ID map |> Option.defaultValue Seq.empty)
                let ref = request.requestDateTime
                let (info, text) =
                    match request.renderOptions with
                    | Image (_, version) ->
                        let profileUrl = 
                            tryFindUserById tweet.AuthorID includes 
                            |> Option.map (fun user -> user.ProfileImageUrl |> Option.ofObj |> Option.defaultValue "") 
                            |> Option.defaultValue ""
                        let info = Some <| AppSettings.ImageInfo(tweet.Source, profileUrl, mockTweet.Date, mockTweet.IsVerified, mockTweet.IsProtected, mockTweet.Name, mockTweet.ScreenName)
                        (info, match version with | Version.Full -> mockTweet.ToFullSpeakText(ref) | Version.Regular -> mockTweet.ToSpeakText(ref))
                    | Video Version.Full | Text Version.Full -> (None, mockTweet.ToFullSpeakText(ref))
                    | Video Version.Regular | Text Version.Regular -> (None, mockTweet.ToSpeakText(ref))
                AppSettings.QueuedTweet(request.requestTweetID, RenderType.fromRenderOption request.renderOptions |> RenderType.serialize, text, request.renderTweetID, info) 
                |> Choice2Of2 
                |> Success
            | _ -> TwitterError (message, exn)
        | OtherError (message, exn) -> OtherError (message, exn)
        |> handleException client request.renderTweetID request.requestTweetID
}


let handleError (client:Client) (error:LinqToTwitter.Common.TwitterError) (request:RenderRequest) =
    
    let text = 
        match error.Title with
        | "Not Found Error" -> 
             "Sorry, this tweet cannot be found to be rendered. Perhaps the tweet is deleted or the account is set to private?"
        | "Authorization Error" -> 
             "Sorry, there was an authorization error when trying to render this tweet"
        | _ -> 
            "Sorry, there was an error from Twitter when trying to render this tweet"

    async {
        let! result = renderText client text request.requestTweetID
        logClientResultError (sprintf "when trying to render tweet ID %s" error.ID) result

        return!
            match result with
            | Success ()        -> Success <| Choice1Of2 request.requestTweetID
            | TwitterError (message, exn) ->
                match exn.StatusCode with
                | Net.HttpStatusCode.TooManyRequests 
                | Net.HttpStatusCode.InternalServerError
                | Net.HttpStatusCode.ServiceUnavailable
                | Net.HttpStatusCode.BadGateway ->
                    AppSettings.QueuedTweet(request.requestTweetID, RenderType.serialize ErrorRender, text, error.ID, None) 
                    |> Choice2Of2 
                    |> Success
                | _ -> TwitterError(message, exn)
            | OtherError (message, exn) -> OtherError (message, exn)
            |> handleException client request.renderTweetID request.requestTweetID
    }

let handleMentions (client:Client) (appsettings:AppSettings.Root) =

    let rec handleMentions' (client:Client) (appsettings:AppSettings.Root) startDate endDate token = async {

        let! queuedResults = appsettings.QueuedTweets |> Seq.map (handleQueuedTweet client) |> Async.Parallel

        let appsettings =
            Array.fold ( fun state -> function 
                | Success (Choice1Of2 requestID) -> AppSettings.Root.dequeueTweet requestID state
                | Success (Choice2Of2 queuedTweet) -> 
                    AppSettings.Root.enqueueTweet queuedTweet state
                    |> AppSettings.Root.setRateLimit endDate
                | _ -> state ) appsettings queuedResults

        match appsettings.RateLimit.Limited with
        | true -> 
            AppSettings.Root.saveAppSettings appsettings
            printfn "App is rate limited until %A" <| Option.get appsettings.RateLimit.NextQueryDateTime
            return ()
        | false ->

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

            let! responseQuery = requests |> ClientResult.map ( Seq.map (fun x -> x.renderTweetID) ) |> ClientResult.bindAsync client.GetTweets

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
                    |> Seq.choose (fun request -> 
                        resp.Tweets
                        |> nullableSequenceToValue
                        |> Seq.tryFind (fun tweet -> request.renderTweetID = tweet.ID)
                        |> Option.map (fun tweet -> handleTweet client tweet resp.Includes ee request)
                        |> Option.orElseWith (fun () -> 
                            resp.Errors
                            |> nullableSequenceToValue
                            |> Seq.tryFind (fun error -> request.renderTweetID = error.ID)
                            |> Option.map (fun error -> handleError client error request))))

            let appsettings =
                match requestHandlers with
                | TwitterError _ | OtherError _ -> AppSettings.Root.setRateLimit endDate appsettings
                | Success _                     -> appsettings

            match appsettings.RateLimit.Limited with
            | true ->
                AppSettings.Root.saveAppSettings appsettings
                printfn "App is rate limited until %A" <| Option.get appsettings.RateLimit.NextQueryDateTime
                return ()
            | false ->

                let! results =
                    match requestHandlers with
                    | Success handlers -> Async.Parallel handlers
                    | _ -> async { return Array.empty }

                let appsettings =
                    Array.fold (fun state -> function
                        | Success (Choice2Of2 queuedTweet) -> 
                            AppSettings.Root.enqueueTweet queuedTweet state
                            |> AppSettings.Root.setRateLimit endDate
                        | _ -> state) appsettings results

                match mentions with
                | Success mentions ->
                    match mentions.Meta with
                    | null -> return ()
                    | meta ->
                        match meta.NextToken with
                        | "" | null -> 
                            appsettings
                            |> AppSettings.Root.clearRateLimit
                            |> AppSettings.Root.setQueryDateTime endDate
                            |> AppSettings.Root.saveAppSettings
                            return ()
                        | token -> 
                            let updatedStartDate = mentions.Tweets |> Seq.find (fun x -> x.ID = meta.NewestID) |> fun x -> x.CreatedAt.Value
                            handleMentions' client (AppSettings.Root.setQueryDateTime updatedStartDate appsettings) startDate endDate (Some token) 
                            |> Async.Start
                | TwitterError (_, exn) ->
                    match exn.StatusCode with
                    | Net.HttpStatusCode.TooManyRequests 
                    | Net.HttpStatusCode.InternalServerError
                    | Net.HttpStatusCode.BadGateway
                    | Net.HttpStatusCode.ServiceUnavailable -> 
                        appsettings
                        |> AppSettings.Root.setRateLimit endDate
                        |> AppSettings.Root.saveAppSettings
                    | _ -> ()
                    return ()
                | OtherError _ -> AppSettings.Root.saveAppSettings appsettings; return ()
    }

    let startDate = appsettings.QueryDateTime |> Option.defaultValue appsettings.DefaultQueryDateTime
    let endDate = DateTime.UtcNow
    let rateDate = appsettings.RateLimit.NextQueryDateTime |> Option.filter (fun _ -> appsettings.RateLimit.Limited) |> Option.defaultValue endDate

    if rateDate <= endDate 
    then 
        handleMentions' client (AppSettings.Root.clearRateLimit appsettings) startDate endDate None
    else async { printfn "App is rate limited until %A" rateDate }


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

    

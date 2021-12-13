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

open Npgsql

[<Literal>]
let private schema = 
    """[
        { "queuedTweets": [ 
                {  
                    "requestTweetID": "a", 
                    "requestScreenName": "a",
                    "renderType": "a",
                    "text": "a",
                    "renderTweetID": "a"
                } 
            ], "defaultStartMentionDateTime": "8/30/2021 12:00:00 AM", "rateLimit": { "limited": false } 
        },
        { "queuedTweets": [ 
                { 
                    "requestTweetID": "a", 
                    "requestScreenName" : "a",
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
            ], "defaultStartMentionDateTime": "8/30/2021 12:00:00 AM", "queryDateTime": "9/13/2021 3:45:00 PM", "rateLimit": { "limited": true, "nextQueryDateTime": "8/30/2021 12:00:00 AM" } }
       ]"""

let private imageColumns = seq {
    "image_info_source"
    "image_info_profile_picture_url"
    "image_info_datetime"
    "image_info_verified"
    "image_info_protected"
    "image_info_name"
    "image_info_screen_name"
}

let private requestRenderColumns = seq {
    "request_tweet_id"
    "request_screen_name"
    "render_type"
    "render_tweet_id"
    "text_"
}

let toInsertQuery tableName columns =
    sprintf "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (request_tweet_id) DO UPDATE SET text_ = @text_, render_type = @render_type"
    <| tableName
    <| String.concat "," columns
    <| String.concat "," (Seq.map (sprintf "@%s") columns)


type AppSettings = JsonProvider<schema, SampleIsList=true>
let buildAppSettings () = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "appsettings.json") |> AppSettings.Load

let loadAppSettingsFromDatabase (connection:NpgsqlConnection) =
    
    use queuedCommand = new NpgsqlCommand(Seq.append requestRenderColumns imageColumns |> String.concat "," |> sprintf "SELECT %s FROM queued_tweets", connection)
    use queuedReader = queuedCommand.ExecuteReader()

    let mutable queuedTweets = [| |]
    while queuedReader.Read() do
        let requestTweetID = queuedReader.GetString(queuedReader.GetOrdinal("request_tweet_id"))
        let requestScreenName = queuedReader.GetString(queuedReader.GetOrdinal("request_screen_name"))
        let renderType = queuedReader.GetString(queuedReader.GetOrdinal("render_type"))
        let renderTweetID = queuedReader.GetString(queuedReader.GetOrdinal("render_tweet_id"))
        let text = queuedReader.GetString(queuedReader.GetOrdinal("text_"))

        let imageInfo =
            Some ()
            |> Option.filter (fun _ -> queuedReader.GetOrdinal("image_info_screen_name") |> queuedReader.IsDBNull)
            |> Option.map (fun _ ->
                let imageInfoSource = queuedReader.GetString(queuedReader.GetOrdinal("image_info_source"))
                let imageInfoProfilePicUrl = queuedReader.GetString(queuedReader.GetOrdinal("image_info_profile_picture_url"))
                let imageInfoDateTime = queuedReader.GetDateTime(queuedReader.GetOrdinal("image_info_datetime"))
                let imageInfoVerified = queuedReader.GetBoolean(queuedReader.GetOrdinal("image_info_verified"))
                let imageInfoProtected = queuedReader.GetBoolean(queuedReader.GetOrdinal("image_info_protected"))
                let imageInfoName = queuedReader.GetString(queuedReader.GetOrdinal("image_info_name"))
                let imageInfoScreenName = queuedReader.GetString(queuedReader.GetOrdinal("image_info_screen_name"))
                AppSettings.ImageInfo(imageInfoSource, imageInfoProfilePicUrl, imageInfoDateTime, imageInfoVerified, imageInfoProtected, imageInfoName, imageInfoScreenName))

        let queuedTweet = AppSettings.QueuedTweet(requestTweetID, requestScreenName, renderType, text, renderTweetID, imageInfo)
        queuedTweets <- queuedTweet |> Array.singleton |> Array.append queuedTweets

    queuedReader.Close()

    use queryingCommand = new NpgsqlCommand("SELECT default_query_datetime, query_datetime, next_limited_query_datetime FROM querying WHERE id = 1", connection)
    use queryingReader = queryingCommand.ExecuteReader()

    queryingReader.Read() |> ignore
    let defaultQueryDateTime = queryingReader.GetDateTime(queryingReader.GetOrdinal("default_query_datetime"))
    let queryDatetime = 
        let index = queryingReader.GetOrdinal("query_datetime") 
        if queryingReader.IsDBNull(index) then None else queryingReader.GetDateTime(index) |> Some
    let nextLimitedQueryDatetime = 
        let index = queryingReader.GetOrdinal("next_limited_query_datetime")
        if queryingReader.IsDBNull(index) then None else queryingReader.GetDateTime(index) |> Some

    let rateLimit = AppSettings.RateLimit(nextLimitedQueryDatetime.IsSome, nextLimitedQueryDatetime)
    queryingReader.Close()

    AppSettings.Root(queuedTweets, defaultQueryDateTime, rateLimit, queryDatetime)

let storeAppSettingsInDatabase (connection:NpgsqlConnection) (appsettings:AppSettings.Root) =
    
    use transaction = connection.BeginTransaction()
    let updateRateLimit = 
        if appsettings.RateLimit.Limited
        then ", next_limited_query_datetime = @next_limited_query_datetime"
        else ""
    use queryingCommand = new NpgsqlCommand(sprintf "UPDATE querying SET query_datetime = @query_datetime%s WHERE id = @id" updateRateLimit, connection, transaction)
    queryingCommand.Parameters.AddWithValue("id", 1) |> ignore

    match appsettings.QueryDateTime with
    | None -> queryingCommand.Parameters.AddWithValue("query_datetime", DBNull.Value)
    | Some qdt -> queryingCommand.Parameters.AddWithValue("query_datetime", qdt)
    |> ignore
    
    appsettings.RateLimit.NextQueryDateTime
    |> Option.iter (fun queryDateTime -> queryingCommand.Parameters.AddWithValue("next_limited_query_datetime", queryDateTime) |> ignore)

    queryingCommand.ExecuteNonQuery() |> ignore

    for queuedTweet in appsettings.QueuedTweets do

        let query = toInsertQuery "queued_tweets" <| seq { yield! requestRenderColumns; yield! (if queuedTweet.ImageInfo.IsSome then imageColumns else Seq.empty) }
        use queuedCommand = new NpgsqlCommand(query, connection, transaction)
        queuedCommand.Parameters.AddWithValue("request_tweet_id", queuedTweet.RequestTweetId) |> ignore
        queuedCommand.Parameters.AddWithValue("request_screen_name", queuedTweet.RequestScreenName) |> ignore
        queuedCommand.Parameters.AddWithValue("render_type", queuedTweet.RenderType) |> ignore
        queuedCommand.Parameters.AddWithValue("render_tweet_id", queuedTweet.RenderTweetId) |> ignore
        queuedCommand.Parameters.AddWithValue("text_", queuedTweet.Text) |> ignore

        queuedTweet.ImageInfo
        |> Option.iter (fun imageInfo ->
            queuedCommand.Parameters.AddWithValue("image_info_source", imageInfo.Source) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_profile_picture_url", imageInfo.ProfilePicture) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_datetime", imageInfo.Date) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_verified", imageInfo.Verified) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_protected", imageInfo.Protected) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_name", imageInfo.Name) |> ignore
            queuedCommand.Parameters.AddWithValue("image_info_screen_name", imageInfo.ScreenName) |> ignore)

        try
            queuedCommand.ExecuteNonQuery() |> ignore
        with exn -> printfn "Error occured when trying to execute query %A" exn

    try
        transaction.Commit()
    with exn -> printfn "Error occurred when trying to save to database %A" exn

       
let clearQueuedTweetFromDatabase connection (requestID:string) =
    use cmd = new NpgsqlCommand("DELETE FROM queued_tweets WHERE request_tweet_id=@request_tweet_id", connection)
    cmd.Parameters.AddWithValue("request_tweet_id", requestID) |> ignore
    try
        cmd.Prepare()
        cmd.ExecuteNonQuery() |> ignore
    with exn -> printfn "Error occurred when trying to save to database %A" exn

type AppSettings.Root with
    static member enqueueTweet (queuedTweet: AppSettings.QueuedTweet) (appsettings:AppSettings.Root)  =
        let queuedTweets =
            appsettings.QueuedTweets
            |> Array.filter (fun x -> x.RequestTweetId <> queuedTweet.RequestTweetId)
            |> Array.append [| queuedTweet |]
        AppSettings.Root(queuedTweets, appsettings.DefaultStartMentionDateTime, appsettings.RateLimit, appsettings.QueryDateTime)
   
    static member dequeueTweet tweetID (appsettings:AppSettings.Root)  =
        let queuedTweets = appsettings.QueuedTweets |> Array.filter (fun x -> x.RequestTweetId <> tweetID)
        AppSettings.Root(queuedTweets, appsettings.DefaultStartMentionDateTime, appsettings.RateLimit, appsettings.QueryDateTime)

    static member clearRateLimit (appsettings:AppSettings.Root) =
        let rateLimit = AppSettings.RateLimit(false, None)
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultStartMentionDateTime, rateLimit, appsettings.QueryDateTime)

    static member setRateLimit (currentDateTime:DateTime) (appsettings:AppSettings.Root) =
        let rateLimit = AppSettings.RateLimit(true, Some <| currentDateTime.AddMinutes 5.0)
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultStartMentionDateTime, rateLimit, appsettings.QueryDateTime)

    static member setQueryDateTime queryDateTime (appsettings:AppSettings.Root)  =
        AppSettings.Root(appsettings.QueuedTweets, appsettings.DefaultStartMentionDateTime, appsettings.RateLimit, Some queryDateTime)

    static member saveAppSettings connection (appsettings:AppSettings.Root) =
        File.WriteAllText("appsettings.json", appsettings.ToString())
        storeAppSettingsInDatabase connection appsettings
        

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

let synthesize text outfile =
    Synthesizer().Synthesize(text, outfile)

let speak text filename =
    Synthesizer().Speak(text, filename)

type RenderRequest = 
    {
        requestTweetID: string
        requestScreenName: string
        requestDateTime: DateTime
        renderTweetID: string
        renderOptions: RenderOptions
    }
    static member init (requestTweetID: string, requestScreenName: string, requestDateTime: DateTime, renderTweetID: string, renderOptions: RenderOptions) =
        {
            requestTweetID = requestTweetID
            requestScreenName = requestScreenName
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
        

let private renderVideo (client:Client) text (tweetID:string) (tweetScreenName:string) = async {
    use tempfile = new TempFile()
    try
        do! Synthesizer().Synthesize(text, tempfile.Path)
        let srtvTweet = AudioTweet (tempfile.Path, "Hello! Your video is here and should be attached. Thank you for using the SRTV bot")
        return! client.ReplyAsync(uint64 tweetID, tweetScreenName, srtvTweet)
    with exn -> return OtherError("Video could not be made", exn)
}

let private renderImage (client:Client) info theme (tweetID: string) (tweetScreenName:string) = async {
    let! image = tweetInfoToImage info theme
    let srtvTweet = ImageTweet (image, "Hello! Your image is here and should be attached. There should also be alt text in the image. If the alt text is too big for the image, it will be tweeted in the replies. Thank you for using the SRTV bot", info.text)
    return! client.ReplyAsync(uint64 tweetID, tweetScreenName, srtvTweet)
}

let private renderText (client:Client) text (tweetID: string) (tweetScreenName: string) = 
    client.ReplyAsync(uint64 tweetID, tweetScreenName, TextTweet text)

let render (client:Client) tweet includes map (request:RenderRequest) =
    let mockTweet =
        MockTweet(tweet, includes, Map.tryFind tweet.ID map |> Option.defaultValue Seq.empty)

    let ref = request.requestDateTime

    match request.renderOptions with
    | Video version ->
        let text = match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
        renderVideo client text request.requestTweetID request.requestScreenName
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
        renderImage client info theme request.requestTweetID request.requestScreenName

    | Text version ->
        let text = 
            match version with | Version.Regular -> mockTweet.ToSpeakText(ref) | Version.Full -> mockTweet.ToFullSpeakText(ref)
            |> sprintf "Hello! Your text is here and should be below. There will be multiple tweets if the text is too many characters. Thank you for using the SRTV bot.\n\n\n%s"
        renderText client text request.requestTweetID request.requestScreenName



let rec handleException (client:Client) (renderTweetID: string) (requestTweetID: string) (requestScreenName: string) = function
    | Success value -> async { return Success value }
    | OtherError (message, exn) ->
        async {
            let text = "Sorry, there was an error when trying to render your tweet"
            let! result = client.ReplyAsync(uint64 requestTweetID, requestScreenName, TextTweet text)
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
                        AppSettings.QueuedTweet(requestTweetID, requestScreenName, RenderType.serialize TextRender, text, renderTweetID, None)
                        |> Choice2Of2
                        |> Success
                    | _ -> TwitterError (message, exn)
                | OtherError (message, exn) -> OtherError (message, exn)
        }
    | TwitterError (message, exn) -> handleException client renderTweetID requestTweetID requestScreenName <| OtherError (message, exn)


let handleQueuedTweet (client:Client) (queuedTweet:AppSettings.QueuedTweet) = async {

    let! result = 
        match RenderType.deserialize queuedTweet.RenderType with
        | Some VideoRender -> renderVideo client queuedTweet.Text queuedTweet.RequestTweetId queuedTweet.RequestScreenName
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
            |> Option.map (fun info -> renderImage client info theme queuedTweet.RequestTweetId queuedTweet.RequestScreenName)
            |> Option.defaultWith (fun () -> async { return OtherError ("Queued tweet info was not available", Failure("Queued tweet info was not available"))})

        | Some TextRender | Some ErrorRender -> renderText client queuedTweet.Text queuedTweet.RequestTweetId queuedTweet.RequestScreenName
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
        |> handleException client queuedTweet.RenderTweetId queuedTweet.RequestTweetId queuedTweet.RequestScreenName
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
                AppSettings.QueuedTweet(request.requestTweetID, request.requestScreenName, RenderType.fromRenderOption request.renderOptions |> RenderType.serialize, text, request.renderTweetID, info) 
                |> Choice2Of2 
                |> Success
            | _ -> TwitterError (message, exn)
        | OtherError (message, exn) -> OtherError (message, exn)
        |> handleException client request.renderTweetID request.requestTweetID request.requestScreenName
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
        let! result = renderText client text request.requestTweetID request.requestScreenName
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
                    AppSettings.QueuedTweet(request.requestTweetID, request.requestScreenName, RenderType.serialize ErrorRender, text, error.ID, None) 
                    |> Choice2Of2 
                    |> Success
                | _ -> TwitterError(message, exn)
            | OtherError (message, exn) -> OtherError (message, exn)
            |> handleException client request.renderTweetID request.requestTweetID request.requestScreenName
    }

let handleMentions (client:Client) connection (appsettings:AppSettings.Root) =

    let rec handleMentions' (client:Client) connection (appsettings:AppSettings.Root) startDate endDate token = async {

        let! queuedResults = appsettings.QueuedTweets |> Seq.map (handleQueuedTweet client) |> Async.Parallel

        let appsettings =
            Array.fold ( fun state -> function 
                | Success (Choice1Of2 requestID) -> 
                    let settings = AppSettings.Root.dequeueTweet requestID state
                    clearQueuedTweetFromDatabase connection requestID
                    settings
                | Success (Choice2Of2 queuedTweet) -> 
                    AppSettings.Root.enqueueTweet queuedTweet state
                    |> AppSettings.Root.setRateLimit endDate
                | _ -> state ) appsettings queuedResults

        match appsettings.RateLimit.Limited with
        | true -> 
            AppSettings.Root.saveAppSettings connection appsettings
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
                        | VideoRenderMention mentions.Includes (requestTweetID, requestScreenName, requestDateTime, fullVersion, renderTweetID) ->
                            Some <| RenderRequest.init (requestTweetID, requestScreenName, requestDateTime, renderTweetID, Video (toVersion fullVersion))
                        | ImageRenderMention mentions.Includes (requestTweetID, requestScreenName, requestDateTime, theme, fullVersion, renderTweetID) -> 
                            Some <| RenderRequest.init (requestTweetID, requestScreenName, requestDateTime, renderTweetID, Image (Theme.fromAttributeValue theme |> Option.defaultValue Theme.Dim, toVersion fullVersion)) 
                        | TextRenderMention mentions.Includes (requestTweetID, requestScreenName, requestDateTime, fullVersion, renderTweetID) ->
                            Some <| RenderRequest.init (requestTweetID, requestScreenName, requestDateTime, renderTweetID, Text (toVersion fullVersion))
                        | GeneralRenderMention mentions.Includes (requestTweetID, requestScreenName, requestDateTime, fullVersion, renderTweetID) ->
                            Some <| RenderRequest.init (requestTweetID, requestScreenName, requestDateTime, renderTweetID, Video (toVersion fullVersion))
                        | _ -> None))

            let! responseQuery = requests |> ClientResult.map ( Seq.map (fun x -> x.renderTweetID) ) |> ClientResult.bindAsync client.GetTweets

            logClientResultError "getting requested renderable tweets" responseQuery

            let! extendedEntities =
                responseQuery
                |> ClientResult.map (fun resp ->
                    resp.Tweets
                    |> nullableSequenceToValue
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
                | TwitterError _ -> AppSettings.Root.setRateLimit endDate appsettings
                | OtherError _ -> AppSettings.Root.setRateLimit (endDate.AddMinutes(-4.2)) appsettings
                | Success _                     -> appsettings

            match appsettings.RateLimit.Limited with
            | true ->
                AppSettings.Root.saveAppSettings connection appsettings
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
                            |> AppSettings.Root.saveAppSettings connection
                            return ()
                        | token -> 
                            let updatedStartDate = mentions.Tweets |> Seq.find (fun x -> x.ID = meta.NewestID) |> fun x -> x.CreatedAt.Value
                            handleMentions' client connection (AppSettings.Root.setQueryDateTime updatedStartDate appsettings) startDate endDate (Some token) 
                            |> Async.Start
                | TwitterError (_, exn) ->
                    match exn.StatusCode with
                    | Net.HttpStatusCode.TooManyRequests 
                    | Net.HttpStatusCode.InternalServerError
                    | Net.HttpStatusCode.BadGateway
                    | Net.HttpStatusCode.ServiceUnavailable -> 
                        appsettings
                        |> AppSettings.Root.setRateLimit endDate
                        |> AppSettings.Root.saveAppSettings connection
                    | _ -> ()
                    return ()
                | OtherError _ -> AppSettings.Root.saveAppSettings connection appsettings; return ()
    }

    let startDate = appsettings.QueryDateTime |> Option.defaultValue appsettings.DefaultStartMentionDateTime
    let endDate = DateTime.UtcNow
    let rateDate = appsettings.RateLimit.NextQueryDateTime |> Option.filter (fun _ -> appsettings.RateLimit.Limited) |> Option.defaultValue endDate

    if rateDate <= endDate 
    then 
        handleMentions' client connection (AppSettings.Root.clearRateLimit appsettings) startDate endDate None
    else async { printfn "App is rate limited until %A" rateDate }


let isDevelopmentEnvironment () =
    let environmentVariable = Environment.GetEnvironmentVariable("NETCORE_ENVIRONMENT")
    String.IsNullOrEmpty(environmentVariable) || environmentVariable.ToLower() = "development"

let fetchTweet (client:Client) tweetID = async {
    let! resp = Seq.singleton tweetID |> client.GetTweets
    let! extendedEntities = Seq.singleton tweetID |> client.getTweetMediaEntities
    return resp |> ClientResult.bind (fun resp -> extendedEntities |> ClientResult.map (fun ee -> (resp, ee)))
}

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
            printfn "Tweet text is %s" tweetQuery.Tweets.[0].Text

            let media = nullableSequenceToValue tweetQuery.Includes.Media
            printfn "Tweet alt text is %s" (Seq.tryHead media |> Option.map (fun x -> x.AltText) |> Option.defaultValue "")
        | TwitterError (message, exn) ->
            printfn "Twitter error message: %s" message
            printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        | OtherError (message, exn) ->
            printfn "Non-Twitter error message: %s" message
            printfn "Error: %O, Stack trace %s" exn exn.StackTrace
    }
   
let buildClient () =
    let builder = ConfigurationBuilder().AddEnvironmentVariables()
    Some ()
    |> Option.filter (fun _ -> isDevelopmentEnvironment())
    |> Option.map (fun _ -> AppDomain.CurrentDomain.FriendlyName |> AssemblyName |> Assembly.Load)
    |> Option.filter (function | null -> false | _ -> true)
    |> Option.map (fun assembly -> builder.AddUserSecrets(assembly, true, true))
    |> Option.orElse (Some builder)
    |> Option.map (fun builder -> builder.Build() |> Client)

    

[<EntryPoint>]
let main argv =

    match argv with
    | [| "mentions" |] -> 
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client ->
            match tryFindEnvironmentVariable "DATABASE_URL" with
            | None -> async { printfn "No PostgreSQL database URL set to connect with database..." }
            | Some url ->
                let uri = Uri(url)
                let userInfo = uri.UserInfo.Split(":")
                let connString = 
                    NpgsqlConnectionStringBuilder(
                        Host = uri.Host,
                        Port = uri.Port,
                        Username = userInfo.[0],
                        Password = userInfo.[1],
                        Database = uri.AbsolutePath.TrimStart('/'),
                        SslMode = SslMode.Require,
                        TrustServerCertificate = true
                    ).ToString()

                async {
                    use conn = new NpgsqlConnection(connString)
                    conn.Open()
                    let appsettings = loadAppSettingsFromDatabase conn
                    do! handleMentions client conn appsettings
                }

    | [| "synthesize"; "-f"; "--tweet_id"; tweetID |] 
    | [| "synthesize"; "--tweet_id"; tweetID |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let! result = 
                result
                |> ClientResult.bindAsync (fun (resp, ee) -> async {
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    use tempFile = new TempFile()
                    let text = if Array.contains "-f" argv then mockTweet.ToFullSpeakText() else mockTweet.ToSpeakText()
                    do! synthesize text tempFile.Path
                    let srtvTweet = AudioTweet (tempFile.Path, "Hello! Your video is here and should be attached. Thank you for using the SRTV bot")
                    return! client.TweetAsync srtvTweet
                })

            match result with
            | Success _ -> printfn "Sending tweet was successful..."
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }

    | [| "synthesize"; "-f"; "--tweet_id"; tweetID; "--outfile"; outfile |] 
    | [| "synthesize"; "--tweet_id"; tweetID; "--outfile"; outfile |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let! result = 
                result
                |> ClientResult.bindAsync (fun (resp, ee) -> async {
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    let text = if Array.contains "-f" argv then mockTweet.ToFullSpeakText() else mockTweet.ToSpeakText()
                    do! synthesize text outfile
                    return Success ()
                })

            match result with
            | Success _ -> printfn "Tweet was successfully saved to %s..." outfile
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }
           
    | [| "synthesize"; text; outfile |] -> synthesize text outfile
    | [| "synthesize"; text |] -> synthesize text <| Path.Join (Environment.CurrentDirectory, "synthesis.mp4")
    | [| "speak"; text; outfile |] -> speak text outfile
    | [| "speak"; text |] -> speak text <| Path.Join (Environment.CurrentDirectory, "speakText.wav")
    | [| "image"; theme; "-f"; "--tweet_id"; tweetID |] 
    | [| "image"; theme; "--tweet_id"; tweetID |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let! result = 
                result
                |> ClientResult.bindAsync (fun (resp, ee) -> async {
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    let text = if Array.contains "-f" argv then mockTweet.ToFullSpeakText() else mockTweet.ToSpeakText()
                    let profileUrl = findUserById resp.Tweets.[0].AuthorID resp.Includes |> fun x -> x.ProfileImageUrl
                    let theme = Theme.fromAttributeValue theme |> Option.defaultValue Theme.Dim
                    let! image = toImage mockTweet profileUrl resp.Tweets.[0].Source DateTime.UtcNow <| Image (theme, if Array.contains "-f" argv then Version.Full else Version.Regular)
                    let srtvTweet = ImageTweet (image, "Hello! Your image is here and should be attached. There should also be alt text in the image. If the alt text is too big for the image, it will be tweeted in the replies. Thank you for using the SRTV bot", text)
                    return! client.TweetAsync srtvTweet
                })

            match result with
            | Success _ -> printfn "Sending tweet was successful..."
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }
    | [| "image"; theme; "-f"; "--tweet_id"; tweetID; "--outfile"; outfile |] 
    | [| "image"; theme; "--tweet_id"; tweetID; "--outfile"; outfile |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let! result = 
                result
                |> ClientResult.bindAsync (fun (resp, ee) -> async {
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    let profileUrl = findUserById resp.Tweets.[0].AuthorID resp.Includes |> fun x -> x.ProfileImageUrl
                    let theme = Theme.fromAttributeValue theme |> Option.defaultValue Theme.Dim
                    let! image = toImage mockTweet profileUrl resp.Tweets.[0].Source DateTime.UtcNow <| Image (theme, if Array.contains "-f" argv then Version.Full else Version.Regular)
                    File.WriteAllBytes(outfile, image)
                    return Success ()
                })

            match result with
            | Success _ -> printfn "Tweet was successfully saved to %s..." outfile
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }
    | [| "sendTweet"; text |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> sendTweet text client
    | [| "getTweet"; id |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> getTweet id client
    | [| "text"; "-f"; "--tweet_id"; tweetID |] | [| "text"; "--tweet_id"; tweetID |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let! result = 
                result
                |> ClientResult.bindAsync (fun (resp, ee) -> async {
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    let text = 
                        if Array.contains "-f" argv then mockTweet.ToFullSpeakText() else mockTweet.ToSpeakText()
                        |> sprintf "Hello! Your text is here and should be below. There will be multiple tweets if the text is too many characters. Thank you for using the SRTV bot.\n\n\n%s"
                    let srtvTweet = TextTweet text
                    return! client.TweetAsync srtvTweet
                })

            match result with
            | Success _ -> printfn "Sending tweet was successful..."
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Exception message: %s" exn.Details
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Exception message: %s" exn.Message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }
    | [| "text"; "-f"; "--tweet_id"; tweetID; "--outfile"; outfile |] 
    | [| "text"; "--tweet_id"; tweetID; "--outfile"; outfile |] ->
        match buildClient() with
        | None -> async { printfn "Client cannot be built..." }
        | Some client -> async {
            let! result = fetchTweet client tweetID
            let result = 
                result
                |> ClientResult.map (fun (resp, ee) ->
                    let mockTweet = MockTweet(resp.Tweets.[0], resp.Includes, ee |> Seq.head |> snd)
                    let text = if Array.contains "-f" argv then mockTweet.ToFullSpeakText() else mockTweet.ToSpeakText()
                    File.WriteAllText(outfile, text))

            match result with
            | Success _ -> printfn "Sending tweet was successful..."
            | TwitterError (message, exn) ->
                printfn "Twitter error message: %s" message
                printfn "Exception message: %s" exn.ReasonPhrase
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace

            | OtherError (message, exn) ->
                printfn "Non-Twitter error message: %s" message
                printfn "Exception message: %s" exn.Message
                printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
        }
    | [| "help"|] ->
        printfn 
            """
                Demo options:

                mentions - Read appsettings.json and render requests in the authenticated account's mentions
                
                synthesize [-f] --tweet_id <tweet_id> [--outfile <outfile>] 
                    - fetches <tweet_id> and renders into a video, uses outfile to save to local file, otherwise the result will be tweeted. Use [-f] switch to render full version of a tweet
                
                synthesize <text> [outfile] - renders <text> to a local file. Can specify the filename with [outfile]
                
                speak <text> [outfile] - renders <text> to a local audio file. Can specify the filename with [outfile]
                
                image <theme> [-f] --tweet_id <tweet_id> [--outfile <outfile>]
                    - fetches <tweet_id> and renders into an image with theme <theme> (dark, light, or dim). Other parameters function the same as in the synthesize option
                
                sendTweet <text> - sends text as a tweet
                
                getTweet <tweet_id> - fetches tweet and prints the ID, text, and image alt text
                
                text [-f] --tweet_id <tweet_id> [--outfile <outfile>]
                    - renders <tweet_id> as text. Other parameters function the same as in the synthesize option
                
                help - Shows this help screen
            """
        async { return () }
    | ps -> async { printfn "Program cannot understand parameters: %s. Type in --help to list options" <| String.concat " | " ps }
    |> Async.RunSynchronously
    printfn "End of program..."
    0

    

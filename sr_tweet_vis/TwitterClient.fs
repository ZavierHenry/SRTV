namespace SRTV

open Utilities

module SRTVResponse = 

    type Audio = string
    type Image = byte []

    type SRTVTweet =
        | AudioTweet of audio:Audio * text:string
        | ImageTweet of image:Image * text:string * altText:string
        | TextTweet of text:string

module TwitterClient =
    open System

    open LinqToTwitter
    open LinqToTwitter.Common
    open LinqToTwitter.OAuth

    open System.IO
    open SRTVResponse
    open System.Text.RegularExpressions

    module Text =
        open SRTV.Regex.Urls

        let [<Literal>] URLLength = 23
        let [<Literal>] MaxTweetLength = 280

        let textLength text = 
            let URLs = extractUrls text
            let sumURLLength = List.sumBy (fun url -> String.length url.url) URLs
            let twitterLength = URLLength * URLs.Length
            text.Length - sumURLLength + twitterLength

        let rec private splitTwitterText' (text:string) index count urls (acc:string list) =
            if index >= text.Length
            then List.rev acc
            else if count > MaxTweetLength
            then
                let lastBoundaryIndex = 
                    Regex.Matches(text.[0 .. (index-1)], @"\s+")
                    |> Seq.tryLast
                    |> Option.map (fun m -> m.Index)
                    |> Option.defaultValue index
                let newText = text.[lastBoundaryIndex..].TrimStart()
                splitTwitterText' newText 0 0 urls (text.[0 .. (lastBoundaryIndex-1)] :: acc)
            else if 
                List.tryHead urls
                |> Option.exists (fun {start = start} -> start = index)
            then 
                let {url = url} = List.head urls
                splitTwitterText' text (index+url.Length) (count + URLLength) (List.tail urls) acc
            else splitTwitterText' text (index+1) (count+1) urls acc        

        let splitTwitterText text = splitTwitterText' text 0 0 (extractUrls text) []


    let private equalsTweetID ID (tweet:Tweet) = tweet.ID = ID
    let private equalsUserID ID (user:TwitterUser) = user.ID = ID
    let private equalsReferenceType t (ref: TweetReference) = ref.Type = t

    let tryFindTweetById ID (includes:TwitterInclude) = Seq.tryFind (equalsTweetID ID) includes.Tweets
    let findTweetById ID (includes:TwitterInclude) = Seq.find (equalsTweetID ID) includes.Tweets
    
    let tryFindUserById ID (includes:TwitterInclude) = Seq.tryFind (equalsUserID ID) includes.Users
    let findUserById ID (includes:TwitterInclude) = Seq.find (equalsUserID ID) includes.Users

    let tryFindTweetReferenceByType t = Seq.tryFind (equalsReferenceType t)
    let findTweetReferenceByType t = Seq.find (equalsReferenceType t)

    type ClientResult<'a> =
        | Success of 'a
        | TwitterError of message:string * exn:exn
        | OtherError of message:string * exn:exn

        static member bind<'a, 'b> (f: 'a -> ClientResult<'b>) =
            function
            | Success x                     -> f x
            | TwitterError (message, exn)   -> TwitterError (message, exn)
            | OtherError (message, exn)     -> OtherError (message, exn)

        static member map<'a, 'b> (f: 'a -> 'b) =
            function
            | Success x                     -> Success (f x)
            | TwitterError (message, exn)   -> TwitterError (message, exn)
            | OtherError (message, exn)     -> OtherError (message, exn) 
            

    type AsyncClientResult<'a> = Async<ClientResult<'a>>

    let bindAsync<'a, 'b> (f: 'a -> AsyncClientResult<'b>) result = async {
        match result with
        | Success x                     -> return! f x
        | TwitterError (message, exn)   -> return TwitterError (message, exn)
        | OtherError (message, exn)     -> return OtherError (message, exn)
    }

    let mapAsync<'a, 'b> (f: 'a -> Async<'b>) result = async {
        match result with
        | Success x                     -> let! b = f x in return Success b
        | TwitterError (message, exn)   -> return TwitterError (message, exn)
        | OtherError (message, exn)     -> return OtherError (message, exn)
    }

    open FSharp.Data
    type JVal = JsonValue
    
    type Client() = 
        let credentialStore = new SingleUserInMemoryCredentialStore()
        let auth = new SingleUserAuthorizer()
        let userID = Environment.GetEnvironmentVariable("user_id")

        do
            credentialStore.AccessToken <- Environment.GetEnvironmentVariable("access_token")
            credentialStore.AccessTokenSecret <- Environment.GetEnvironmentVariable("access_token_secret")
            credentialStore.ConsumerKey <- Environment.GetEnvironmentVariable("consumer_key")
            credentialStore.ConsumerSecret <- Environment.GetEnvironmentVariable("consumer_secret")
            auth.CredentialStore <- credentialStore

        let context = new TwitterContext(auth)

        member this.makeTwitterCall<'a> failureMessage (thunk:unit -> Async<'a>) = async {
            try
                let! result = thunk ()
                return Success result
            with
            | exn   -> return TwitterError (failureMessage, exn)
        }

        member this.makeTwitterListQuery<'a> failure (thunkQuery:unit ->Linq.IQueryable<'a>) =
            let call() = async {
                let query = query {
                    for q in thunkQuery () do
                        select q
                }
                let! queryList = query.ToListAsync() |> Async.AwaitTask
                return Seq.toList queryList
            }
            this.makeTwitterCall failure call

        member this.makeTwitterSingleQuery<'a> failureMessage (thunkQuery:unit -> Linq.IQueryable<'a>) =
            let call() = async {
                let query = query {
                    for q in thunkQuery () do 
                        select q
                        headOrDefault
                }
                return query
            }
            this.makeTwitterCall failureMessage call
                

        member this.GetMentions(lastQueriedTime: DateTime, ?paginationToken:string) =
            let query () = query {
                for tweet in context.Tweets do
                    where (tweet.Type = TweetType.MentionsTimeline
                        && tweet.ID = userID
                        && tweet.EndTime = lastQueriedTime
                        && Option.forall (fun token -> token = tweet.PaginationToken) paginationToken
                        )
                    select tweet
            }

            this.makeTwitterListQuery $"Problem getting mentions after the last queried time ${lastQueriedTime.ToLongTimeString()}" query

        member private this.uploadAudioAsync(filename: Audio) =
            try
                File.ReadAllBytes(filename) |> Success
            with
            | ex -> OtherError ("Error retrieving audio file", ex)

            |> bindAsync (this.uploadMediaAsync "video/mp4" "tweet_video")

        member private this.uploadImageAsync(image: Image) =
            let mime = "image/jpg"
            let mediaCategory = "tweet_image"
            this.uploadMediaAsync mime mediaCategory image

        member private this.uploadMediaAsync mime mediaCategory media : AsyncClientResult<uint64> = async {
            let twitterCall() = context.UploadMediaAsync(media, mime, mediaCategory) |> Async.AwaitTask
            let! result = this.makeTwitterCall $"""Error uploading {Regex.Match(mediaCategory, @"^tweet_(\w+)").Groups.[1]} to Twitter""" twitterCall
            return ClientResult.map (fun (media:Media) -> media.MediaID) result
        }

        member private this.uploadImageAltText text mediaID = async {
            let twitterCall() = context.CreateMediaMetadataAsync(mediaID, text) |> Async.AwaitTask
            return! this.makeTwitterCall "Error uploading image alt text to Twitter" twitterCall
        }

        member this.sendTweetAsync(text: string, ?mediaID: uint64) =
            let twitterCall() = 
                match mediaID with
                    | None -> context.TweetAsync(text, TweetMode.Extended)
                    | Some mediaID -> context.TweetAsync(text, [mediaID], TweetMode.Extended)
                |> Async.AwaitTask
            this.makeTwitterCall "Problem sending a tweet" twitterCall

        member this.sendReplyAsync(replyID: uint64, text: string, ?mediaID: uint64) =
            let twitterCall() = 
                match mediaID with
                    | None -> context.ReplyAsync(replyID, text)
                    | Some mediaID -> context.ReplyAsync(replyID, text, [mediaID], TweetMode.Extended)
                |> Async.AwaitTask
            this.makeTwitterCall $"Problem sending a reply to ID {replyID}" twitterCall

        member this.handleReplyAsync(originalTweetID: uint64, text: string list, ?mediaID: uint64) = async {
            match (text, mediaID) with
                | ([], _)        -> return Success ()
                | (t :: rest, Some mediaID) ->
                    let! result = this.sendReplyAsync(originalTweetID, t, mediaID)
                    return! bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, rest)) result
                | (t :: rest, None) ->
                    let! result = this.sendReplyAsync(originalTweetID, t)
                    return! bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, rest)) result
        }

        member this.handleTweetAsync(text: string list, ?mediaID: uint64) = async {
            match text with
            | []        -> return Success ()
            | t :: rest ->
                let! result = match mediaID with | Some mediaID -> this.sendTweetAsync(t, mediaID) | None -> this.sendTweetAsync(t)
                return! bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, rest)) result
        }

        member this.ReplyAsync(originalTweetID: uint64, tweet: SRTVTweet) = async {
            match tweet with
            | AudioTweet (audio, text) ->
                let! mediaID = this.uploadAudioAsync audio
                let splitText = Text.splitTwitterText text
                return! bindAsync (fun mediaID -> this.handleReplyAsync(originalTweetID, splitText, mediaID)) mediaID
            | ImageTweet (image, text, altText) ->
                let! mediaID = this.uploadImageAsync image
                let boundFunction mediaID = async { 
                    let! result = this.uploadImageAltText altText mediaID
                    return ClientResult.map (fun () -> mediaID) result
                }
                let! result = bindAsync boundFunction mediaID
                let splitText = Text.splitTwitterText text
                return! bindAsync (fun mediaID -> this.handleReplyAsync(originalTweetID, splitText, mediaID)) result
            | TextTweet text ->
                let splitText = Text.splitTwitterText text
                return! this.handleReplyAsync(originalTweetID, splitText)       
        }

        member this.TweetAsync(tweet: SRTVTweet) = async {
            match tweet with
            | AudioTweet (audio, text) ->
                let! mediaID = this.uploadAudioAsync audio
                let splitText = Text.splitTwitterText text
                return! bindAsync (fun mediaID -> this.handleTweetAsync(splitText, mediaID)) mediaID
            | ImageTweet (image, text, altText) ->
                let! mediaID = this.uploadImageAsync image
                let boundFunction mediaID = async {
                    let! result = this.uploadImageAltText altText mediaID
                    return ClientResult.map (fun () -> mediaID) result
                }
                let! result = bindAsync boundFunction mediaID
                let splitText = Text.splitTwitterText text
                return! bindAsync (fun mediaID -> this.handleTweetAsync(splitText, mediaID)) result
            | TextTweet text ->
                let splitText = Text.splitTwitterText text
                return! this.handleTweetAsync(splitText)
        }

        //Assumes that dm text is below Twitter limit of 10000 characters
        //Shouldn't be a problem for now given alt text limits of 1000 and tweet limit of 280
        member this.SendDirectMessage(recipient: uint64, dm: SRTVTweet) = async {
            let makeTwitterCall (call: unit -> Async<'a>) = 
                this.makeTwitterCall $"Problem sending direct message to ID {recipient}" call 
            
            match dm with
            | AudioTweet (audio, text) ->
                let! mediaID = this.uploadAudioAsync audio
                let boundFunction (mediaID:uint64) =
                    let call() = context.NewDirectMessageEventAsync(recipient, text, mediaID) |> Async.AwaitTask
                    makeTwitterCall call
                return! bindAsync boundFunction mediaID
            | ImageTweet (image, text, altText) ->
                let! mediaID = this.uploadImageAsync image
                let boundFunction mediaID = async {
                    let! result = this.uploadImageAltText altText mediaID
                    return ClientResult.map (fun () -> mediaID) result
                }    
                let! result = bindAsync boundFunction mediaID
                let func (mediaID:uint64) =
                    let call() = context.NewDirectMessageEventAsync(recipient, text, mediaID) |> Async.AwaitTask
                    makeTwitterCall call
                return! bindAsync func result
            | TextTweet text ->
                let call() = context.NewDirectMessageEventAsync(recipient, text) |> Async.AwaitTask
                return! makeTwitterCall call
        }

        member this.getTweetMediaEntities(tweetIDs: uint64 seq) =
            let ids = Seq.map string tweetIDs |> String.concat ","

            let query () = query {
                for tweet in context.Status do
                    where (tweet.Type = StatusType.Mentions
                        && tweet.TweetIDs = ids
                    )
                    select (tweet.ID, tweet.ExtendedEntities.MediaEntities |> Seq.toList)
                }

            this.makeTwitterListQuery $"Problem trying to retrieve extended entities for {tweetIDs}" query

        member this.rawQueryAsync (p:string) (url:string) =
            let queryString = $"{url}?{p}"

            let q () = query {
                for raw in context.RawQuery do
                    where (raw.QueryString = queryString)
                    select raw.Response
                }

            this.makeTwitterSingleQuery $"Problem retrieving raw request from {url} with parameters {p}" q



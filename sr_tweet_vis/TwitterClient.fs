namespace SRTV

open Utilities
open System.Text.RegularExpressions

module SRTVResponse = 

    type Audio = string
    type Image = byte []

    type SRTVTweet =
        | AudioTweet of audio:Audio * text:string
        | ImageTweet of image:Image * text:string * altText:string
        | TextTweet of text:string


module Twitter =
    open LinqToTwitter
    open LinqToTwitter.Common
    open LinqToTwitter.OAuth

    let private equalsTweetID ID (tweet:Tweet) = tweet.ID = ID
    let private equalsUserID ID (user:TwitterUser) = user.ID = ID
    let private equalsReferenceType t (ref: TweetReference) = ref.Type = t

    let tryFindTweetById ID (includes:TwitterInclude) = Seq.tryFind (equalsTweetID ID) includes.Tweets
    let findTweetById ID (includes:TwitterInclude) = Seq.find (equalsTweetID ID) includes.Tweets
    
    let tryFindUserById ID (includes:TwitterInclude) = Seq.tryFind (equalsUserID ID) includes.Users
    let findUserById ID (includes:TwitterInclude) = Seq.find (equalsUserID ID) includes.Users

    let tryFindTweetReferenceByType t = Seq.tryFind (equalsReferenceType t)
    let findTweetReferenceByType t = Seq.find (equalsReferenceType t)

    module Patterns =

        module Mentions =

            let currentTime () = System.DateTime.UtcNow

            let tryFindReply (tweet:Tweet) = tryFindTweetReferenceByType "replied_to" tweet.ReferencedTweets
            let tryFindQuoteTweet (tweet:Tweet) = tryFindTweetReferenceByType "quoted" tweet.ReferencedTweets
            
            let matchTextPattern pattern (tweet:Tweet) = Regex.Match(tweet.Text, pattern)
            
            let renderMention pattern response = 
                let renderableMatch = 
                    Some response 
                    |> Option.map (matchTextPattern $@"(\s|^)render(?:\s+(?<full>full)\s+)?(%s{pattern})(\s|$|\?|\.)")
                    |> Option.filter (fun m -> m.Success)

                match renderableMatch with
                | Some _ ->
                    renderableMatch
                    |> Option.bind ( fun m -> tryFindReply response |> Option.map (fun ref -> (m, ref)) )
                    |> Option.orElse (renderableMatch |> Option.bind (fun m -> tryFindQuoteTweet response |> Option.map (fun ref -> (m, ref)) ))
                | None -> None

            let (|VideoRenderMention|_|) includes (response:Tweet) = 
                let screenName = tryFindUserById response.AuthorID includes |> Option.map (fun user -> user.Username) |> Option.defaultValue ""
                renderMention "\s+(?:video|sound|audio)" response
                |> Option.map (fun (m, ref) -> 
                    (response.ID, screenName, response.CreatedAt.GetValueOrDefault(currentTime()), m.Groups.["full"].Success, ref.ID))

            let (|TextRenderMention|_|) includes (response:Tweet) = 
                let screenName = tryFindUserById response.AuthorID includes |> Option.map (fun user -> user.Username) |> Option.defaultValue ""
                renderMention "\s+text" response
                |> Option.map (fun (m, ref) -> (response.ID, screenName, response.CreatedAt.GetValueOrDefault(currentTime()), m.Groups.["full"].Success, ref.ID))

            let (|ImageRenderMention|_|) includes (response:Tweet) = 
                let screenName = tryFindUserById response.AuthorID includes |> Option.map (fun user -> user.Username) |> Option.defaultValue ""
                renderMention @"\s+((?<theme>light|dim|dark)\s+)?image" response
                |> Option.map (fun (m, ref) -> (response.ID, screenName, response.CreatedAt.GetValueOrDefault(currentTime()), m.Groups.["theme"].Value, m.Groups.["full"].Success, ref.ID))

            let (|GeneralRenderMention|_|) includes = function
                | VideoRenderMention includes _ -> None
                | TextRenderMention includes _ -> None
                | ImageRenderMention includes _ -> None
                | response ->
                    let screenName = tryFindUserById response.AuthorID includes |> Option.map (fun user -> user.Username) |> Option.defaultValue ""
                    renderMention "" response
                    |> Option.map (fun (m, ref) -> (response.ID, screenName, response.CreatedAt.GetValueOrDefault(currentTime()), m.Groups.["full"].Success, ref.ID))

        let (|PrivateTweet|_|) includes (tweet:Tweet) =
            tryFindUserById tweet.InReplyToUserID includes
            |> Option.filter (fun user -> user.Protected)

        let (|QuotedTweet|_|) includes (tweet:Tweet) =
            tryFindTweetReferenceByType "quoted" tweet.ReferencedTweets
            |> Option.bind (fun ref -> tryFindTweetById ref.ID includes)
            |> Option.map (fun original -> (original, tweet))

        let (|Retweet|_|) includes (tweet:Tweet) =
            tryFindTweetReferenceByType "retweeted" tweet.ReferencedTweets
            |> Option.bind (fun ref -> tryFindTweetById ref.ID includes)
            |> Option.map (fun original -> (original, (findUserById tweet.ID includes).Name))

        let (|Reply|_|) (tweet:Tweet) = 
            let mentions = tweet.Entities.Mentions |> Seq.sortBy (fun mention -> mention.Start)
            let repliedTo =
                Seq.indexed mentions
                |> Seq.takeWhile ( fun (index, mention) -> 
                    Seq.tryItem (index-1) mentions 
                    |> Option.filter (fun m -> m.End + 1 = mention.Start) 
                    |> Option.orElse (Some mention)
                    |> Option.filter (fun m -> index = 0 && m.Start = 0)
                    |> Option.isSome )
                |> Seq.map (fun (_, mention) -> mention.Username)

            Some (tweet, repliedTo) |> Option.filter (fun (_, repliedTo) -> not <| Seq.isEmpty repliedTo)

        let (|MediaTweet|_|) (tweet:Tweet) =
            Some tweet |> Option.filter (fun tweet -> not <| Seq.isEmpty tweet.Attachments.MediaKeys)


    module TwitterClient =
        open System

        open System.IO
        open SRTVResponse
        open Microsoft.Extensions.Configuration

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
                then List.rev (text :: acc)
                else if count > MaxTweetLength
                then
                    let lastBoundaryIndex = 
                        Regex.Matches(text.[0 .. (index-1)], @"\s+")
                        |> Seq.tryLast
                        |> Option.map (fun m -> m.Index)
                        |> Option.defaultValue index
                    let newText = text.[lastBoundaryIndex..].TrimStart()
                    splitTwitterText' newText 0 0 urls (text.[0 .. (lastBoundaryIndex-1)] :: acc)
                else if List.tryHead urls |> Option.exists (fun {start = start} -> start = index)
                then 
                    let {url = url} = List.head urls
                    splitTwitterText' text (index+url.Length) (count + URLLength) (List.tail urls) acc
                else splitTwitterText' text (index+1) (count+1) urls acc        

            let splitTwitterText text = splitTwitterText' text 0 0 (extractUrls text) []

        type ClientResult<'a> =
            | Success of 'a
            | TwitterError of message:string * exn:TwitterQueryException
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
        
        type ClientResult<'a> with
            static member bindAsync<'a, 'b> (f: 'a -> AsyncClientResult<'b>) result = async {
                match result with
                | Success x                     -> return! f x
                | TwitterError (message, exn)   -> return TwitterError (message, exn)
                | OtherError (message, exn)     -> return OtherError (message, exn)
            }

            static member mapAsync<'a, 'b> (f: 'a -> AsyncClientResult<'b>) result = async {
                match result with
                | Success x                     -> let! b = f x in return Success b
                | TwitterError (message, exn)   -> return TwitterError (message, exn)
                | OtherError (message, exn)     -> return OtherError (message, exn)
            }

        open FSharp.Data
        type JVal = JsonValue
    
        type Client(config:IConfiguration) = 
            let credentialStore = new SingleUserInMemoryCredentialStore()
            let auth = new SingleUserAuthorizer()
            //let userID = Environment.GetEnvironmentVariable("user_id")

            let userID = config.GetValue("TWITTER_USER_ID", config.GetValue("TwitterAccount:UserID"))

            let toParams = String.concat ","

            let userFields = seq {
                UserField.Name
                UserField.UserName
                UserField.Protected
                UserField.Verified
                UserField.ProfileImageUrl
            }

            let tweetFields = seq {
                TweetField.Text
                TweetField.Attachments
                TweetField.AuthorID
                TweetField.CreatedAt
                TweetField.Entities
                TweetField.ReferencedTweets
                TweetField.Source
            }

            let expansions = seq {
                ExpansionField.ReferencedTweetAuthorID
                ExpansionField.ReferencedTweetID
                ExpansionField.MediaKeys
                ExpansionField.PollIds
                ExpansionField.AuthorID
                ExpansionField.InReplyToUserID
            }

            let pollFields = seq {
                PollField.Options
                PollField.EndDateTime
            }

            let mediaFields = seq  {
                MediaField.Type
                MediaField.MediaKey
                MediaField.AltText
            }

            do
                credentialStore.AccessToken <- config.GetValue("TWITTER_ACCESS_TOKEN", config.GetValue("TwitterAccount:AccessToken"))
                credentialStore.AccessTokenSecret <- config.GetValue("TWITTER_ACCESS_TOKEN_SECRET", config.GetValue("TwitterAccount:AccessTokenSecret"))
                credentialStore.ConsumerKey <- config.GetValue("TWITTER_CONSUMER_KEY", config.GetValue("TwitterAccount:ConsumerKey"))
                credentialStore.ConsumerSecret <- config.GetValue("TWITTER_CONSUMER_SECRET", config.GetValue("TwitterAccount:ConsumerSecretKey"))
                auth.CredentialStore <- credentialStore

            let context = new TwitterContext(auth)

            member this.makeTwitterCall<'a> failureMessage (thunk:unit -> Async<'a>) = async {
                try
                    let! result = thunk ()
                    return Success result
                with
                | :? TwitterQueryException as exn -> return TwitterError (failureMessage, exn)
                | exn   -> return OtherError (failureMessage, exn)
            }
            
            member this.makeTwitterListQuery<'a> failure (thunkQuery:unit -> Linq.IQueryable<'a>) =
                let call() = async {
                    return thunkQuery() |> Seq.cast<'a> |> Seq.toList
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
                

            member this.GetMentions(startTime: DateTime, endTime: DateTime, ?paginationToken:string) =
                let query () = query {
                    for tweet in context.Tweets do
                        where (
                            tweet.Type = TweetType.MentionsTimeline &&
                            tweet.ID = userID &&
                            tweet.StartTime >= startTime &&
                            tweet.EndTime <= endTime &&
                            tweet.TweetFields = toParams tweetFields &&
                            tweet.UserFields = toParams userFields &&
                            tweet.Expansions = toParams expansions &&
                            tweet.PollFields = toParams pollFields &&
                            Option.forall (fun token -> match tweet.PaginationToken with | null -> false | paginationToken -> token = paginationToken) paginationToken
                            )
                        select tweet
                }

                this.makeTwitterSingleQuery (sprintf "Problem getting mentions between %A and %A" startTime endTime) query

            member this.GetTweets(ids: string seq) =
                let ids = String.concat "," ids
                let query() = query {
                    for tweet in context.Tweets do
                        where (
                            tweet.Type = TweetType.Lookup &&
                            tweet.Ids = ids &&
                            tweet.TweetFields = toParams tweetFields &&
                            tweet.UserFields = toParams userFields &&
                            tweet.Expansions = toParams expansions &&
                            tweet.PollFields = toParams pollFields &&
                            tweet.MediaFields = toParams mediaFields
                        )
                        select tweet
                }

                this.makeTwitterSingleQuery (sprintf "Problem getting tweets with ids %s" ids) query 

            member this.GetUserByScreenName(screenName:string) =
                let query () = query {
                    for user in context.TwitterUser do
                        where ( 
                            user.Type = UserType.UsernameLookup && 
                            user.Usernames = screenName &&
                            user.UserFields = toParams userFields
                            )
                        select user
                }

                this.makeTwitterSingleQuery $"Problem getting user with ID {userID}" query

            member private this.uploadAudioAsync(filename: Audio) = async {
                return!
                    try
                        File.ReadAllBytes(filename) |> Success
                    with
                    | ex -> OtherError ("Error retrieving audio file", ex)

                    |> ClientResult.bindAsync (this.uploadMediaAsync "video/mp4" "tweet_video")
            }
                

            member private this.uploadImageAsync(image: Image) =
                let mime = "image/jpg"
                let mediaCategory = "tweet_image"
                this.uploadMediaAsync mime mediaCategory image

            member private this.uploadMediaAsync mime mediaCategory media : AsyncClientResult<uint64> = async {
                let twitterCall () = async {
                    let! media = context.UploadMediaAsync(media, mime, mediaCategory) |> Async.AwaitTask
                    match Option.ofObj media.ProcessingInfo with
                    | None -> ()
                    | Some processingInfo ->
                        let rec processState (info:MediaProcessingInfo) =
                            match info.State with
                            | "succeeded" | null -> ()
                            | "in_progress" | "pending" ->
                                 Threading.Thread.Sleep(1000 * processingInfo.CheckAfterSeconds)
                                 let newInfo = query {
                                    for m in context.Media do
                                        where (m.Type = Nullable(MediaType.Status) && m.MediaID = media.MediaID && match media.ProcessingInfo with | null -> false | _ -> true)
                                        select m.ProcessingInfo
                                        headOrDefault
                                 }
                                 processState newInfo
                            | "failed" | _ -> failwith "Media failed to upload to twitter"
                        processState processingInfo
                    return media.MediaID
                }
                return! this.makeTwitterCall $"""Error uploading {Regex.Match(mediaCategory, @"^tweet_(\w+)").Groups.[1]} to Twitter""" twitterCall
            }

            member private this.uploadImageAltText text mediaID = async {
                let twitterCall () = context.CreateMediaMetadataAsync(mediaID, text) |> Async.AwaitTask
                return! this.makeTwitterCall "Error uploading image alt text to Twitter" twitterCall
            }

            member this.sendTweetAsync(text: string, ?mediaID: uint64) =
                let twitterCall() = 
                    match mediaID with
                        | None -> context.TweetAsync(text, TweetMode.Extended)
                        | Some mediaID -> context.TweetAsync(text, [| mediaID |], TweetMode.Extended)
                    |> Async.AwaitTask
                this.makeTwitterCall "Problem sending a tweet" twitterCall

            member this.sendReplyAsync(replyID: uint64, screenName: string, text: string, ?mediaID: uint64) =
                let twitterCall() = 
                    let text = sprintf "@%s %s" screenName text
                    match mediaID with
                        | None -> context.ReplyAsync(replyID, text)
                        | Some mediaID -> context.ReplyAsync(replyID, text, [| mediaID |])
                    |> Async.AwaitTask
                this.makeTwitterCall $"Problem sending a reply to ID {replyID}" twitterCall

            member this.handleReplyAsync(originalTweetID: uint64, screenName: string, text: string list, ?mediaID: uint64) = async {
                match (text, mediaID) with
                    | ([], _)        -> return Success ()
                    | (t :: rest, Some mediaID) ->
                        let! result = this.sendReplyAsync(originalTweetID, screenName, t, mediaID)
                        return! ClientResult.bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, status.User.ScreenName, rest)) result
                    | (t :: rest, None) ->
                        let! result = this.sendReplyAsync(originalTweetID, screenName, t)
                        return! ClientResult.bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, status.User.ScreenName, rest)) result
            }

            member this.handleTweetAsync(text: string list, ?mediaID: uint64) = async {
                match text with
                | []        -> return Success ()
                | t :: rest ->
                    let! result = match mediaID with | Some mediaID -> this.sendTweetAsync(t, mediaID) | None -> this.sendTweetAsync(t)
                    return! ClientResult.bindAsync (fun (status:Status) -> this.handleReplyAsync(status.ID, status.User.ScreenName, rest)) result
            }

            member this.ReplyAsync(originalTweetID: uint64, screenName: string, tweet: SRTVTweet) = async {
                match tweet with
                | AudioTweet (audio, text) ->
                    let! mediaID = this.uploadAudioAsync audio
                    let splitText = Text.splitTwitterText text
                    return! ClientResult.bindAsync (fun mediaID -> this.handleReplyAsync(originalTweetID, screenName, splitText, mediaID)) mediaID
                | ImageTweet (image, text, altText) when Text.textLength altText > 1000 ->
                    let! mediaID = this.uploadImageAsync image
                    let splitText = sprintf "%s %s" text altText |> Text.splitTwitterText
                    return! ClientResult.bindAsync (fun mediaID -> this.handleReplyAsync(originalTweetID, screenName, splitText, mediaID)) mediaID
                | ImageTweet (image, text, altText) ->
                    let! mediaID = this.uploadImageAsync image
                    let boundFunction mediaID = async { 
                        let! result = this.uploadImageAltText altText mediaID
                        return ClientResult.map (fun () -> mediaID) result
                    }
                    let! result = ClientResult.bindAsync boundFunction mediaID
                    let splitText = Text.splitTwitterText text
                    return! ClientResult.bindAsync (fun mediaID -> this.handleReplyAsync(originalTweetID, screenName, splitText, mediaID)) result
                | TextTweet text ->
                    let splitText = Text.splitTwitterText text
                    return! this.handleReplyAsync(originalTweetID, screenName, splitText)       
            }

            member this.TweetAsync(tweet: SRTVTweet) = async {
                match tweet with
                | AudioTweet (audio, text) ->
                    let! mediaID = this.uploadAudioAsync audio
                    let splitText = Text.splitTwitterText text
                    return! ClientResult.bindAsync (fun mediaID -> this.handleTweetAsync(splitText, mediaID)) mediaID
                | ImageTweet (image, text, altText) when Text.textLength altText > 1000 ->
                    let! mediaID = this.uploadImageAsync image
                    let splitText = sprintf "%s %s" text altText |> Text.splitTwitterText
                    return! ClientResult.bindAsync (fun mediaID -> this.handleTweetAsync(splitText, mediaID)) mediaID
                | ImageTweet (image, text, altText) ->
                    let! mediaID = this.uploadImageAsync image
                    let boundFunction mediaID = async {
                        let! result = this.uploadImageAltText altText mediaID
                        return ClientResult.map (fun () -> mediaID) result
                    }
                    let! result = ClientResult.bindAsync boundFunction mediaID
                    return! ClientResult.bindAsync (fun mediaID -> this.handleTweetAsync(Text.splitTwitterText text, mediaID)) result
                | TextTweet text ->
                    return! this.handleTweetAsync(Text.splitTwitterText text)
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
                    return! ClientResult.bindAsync boundFunction mediaID
                | ImageTweet (image, text, altText) ->
                    let! mediaID = this.uploadImageAsync image
                    let boundFunction mediaID = async {
                        let! result = this.uploadImageAltText altText mediaID
                        return ClientResult.map (fun () -> mediaID) result
                    }    
                    let! result = ClientResult.bindAsync boundFunction mediaID
                    let func (mediaID:uint64) =
                        let call() = context.NewDirectMessageEventAsync(recipient, text, mediaID) |> Async.AwaitTask
                        makeTwitterCall call
                    return! ClientResult.bindAsync func result
                | TextTweet text ->
                    let call() = context.NewDirectMessageEventAsync(recipient, text) |> Async.AwaitTask
                    return! makeTwitterCall call
            }

            member this.getTweetMediaEntities(tweetIDs: string seq) =
                let ids = String.concat "," tweetIDs

                let query () = query {
                    for tweet in context.Status do
                        where (tweet.Type = StatusType.Lookup
                            && tweet.TweetIDs = ids
                            && tweet.TrimUser = true
                            && tweet.IncludeAltText = true
                            && tweet.IncludeEntities = true
                        )
                        //select (tweet.ID, match tweet.ExtendedEntities with | null -> [] | extendedEntities -> extendedEntities.MediaEntities |> Seq.toList)
                        let extendedEntities = 
                            Option.ofObj tweet.ExtendedEntities
                            |> Option.map (fun x -> nullableSequenceToValue x.MediaEntities)
                            |> Option.defaultValue Seq.empty

                        let quotedEntities = 
                            Option.ofObj tweet.QuotedStatus
                            |> Option.bind (fun quotedStatus -> Option.ofObj quotedStatus.ExtendedEntities)
                            |> Option.map (fun x -> nullableSequenceToValue x.MediaEntities)
                            |> Option.defaultValue Seq.empty
                        
                        select (tweet.StatusID, Seq.append extendedEntities quotedEntities |> Seq.toList)
                    }

                this.makeTwitterListQuery $"Problem trying to retrieve extended entities for {ids}" query
                



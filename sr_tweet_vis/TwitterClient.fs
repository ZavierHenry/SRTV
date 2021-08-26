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
            open System.Text.RegularExpressions

            let isReply = Option.filter (fun (tweet:Tweet) -> tryFindTweetReferenceByType "replied_to" tweet.ReferencedTweets |> Option.isSome)
            let isQuoteTweet = Option.filter (fun (tweet:Tweet) -> tryFindTweetReferenceByType "quoted" tweet.ReferencedTweets |> Option.isSome)
            
            let hasText pattern = Option.filter ( fun (tweet:Tweet) -> Regex.IsMatch(tweet.Text, pattern) )

            let renderMention pattern response = 
                Some response 
                |> isReply
                |> Option.orElse (isQuoteTweet <| Some response)
                |> hasText $@"(\s|^)render\s+({pattern})(\s|$)"

            let (|VideoRenderMention|_|) response = renderMention "video|sound|audio" response
            let (|TextRenderMention|_|) response = renderMention "text" response
            let (|ImageRenderMention|_|) response = 
                let pattern = @"((?<theme>light|dim|dark)\s+)?image"
                renderMention pattern response
                |> Option.map ( fun tweet -> (tweet, Regex.Match(tweet.Text, pattern).Groups.["theme"].Value) )

            let (|GeneralRenderMention|_|) = function
                | VideoRenderMention _ -> None
                | TextRenderMention _ -> None
                | ImageRenderMention _ -> None
                | response -> Some response |> isReply |> hasText @"(\s|^)render(\s|$)"


        let (|AuthorizationError|_|) (response:TweetQuery) =
            Some ()
            |> Option.filter ( fun _ -> response.Errors |> Seq.exists (fun error -> error.Title = "Authorization Error" ) )

        let (|NotFoundError|_|) (response:TweetQuery) =
            Some ()
            |> Option.filter ( fun _ -> response.Errors |> Seq.exists (fun error -> error.Title = "Not Found Error") )

        let (|UnavailableReplyRequest|_|) (response:TweetQuery) (tweet:Tweet) = 
            tryFindTweetReferenceByType "replied_to" tweet.ReferencedTweets
            |> Option.bind (fun ref -> response.Errors |> Seq.tryFind (fun err -> err.Value = ref.ID))
            |> Option.map (fun _ -> ())

        let (|UnavailableQuoteTweetRequest|_|) (response:TweetQuery) (tweet:Tweet) = 
            if tweet.ReferencedTweets |> Seq.exists (fun ref -> ref.Type = "quoted")
            then None
            else 
                match Regex.Match(tweet.Text, @"https://t\.co/\w+$") with
                | m when m.Success ->
                    tweet.Entities.Urls
                    |> Seq.tryFind (fun x -> x.Url = m.Value)
                    |> Option.map (fun x -> Regex.Match(x.ExpandedUrl, @"https://twitter.com/\w+?/status/(\d+)"))
                    |> Option.map (fun _ -> ())
                | _ -> None

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
        open System.Text.RegularExpressions

        open Microsoft.Extensions.Configuration
        open Microsoft.Extensions.Hosting

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
    
        type Client(config:IConfiguration) = 
            let credentialStore = new SingleUserInMemoryCredentialStore()
            let auth = new SingleUserAuthorizer()
            //let userID = Environment.GetEnvironmentVariable("user_id")

            let userID = config.GetValue("TwitterAccount:UserID")

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
            }

            let pollFields = seq {
                PollField.Options
                PollField.EndDateTime
            }

            let mediaFields = seq  {
                MediaField.Type
                MediaField.MediaKey
                "alt_text"
            }

            do
                credentialStore.AccessToken <- config.GetValue("TwitterAccount:AccessToken")
                credentialStore.AccessTokenSecret <- config.GetValue("TwitterAccount:AccessTokenSecret")
                credentialStore.ConsumerKey <- config.GetValue("TwitterAccount:ConsumerKey")
                credentialStore.ConsumerSecret <- config.GetValue("TwitterAccount:ConsumerSecretKey")
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
                        where (
                            tweet.Type = TweetType.MentionsTimeline &&
                            tweet.ID = userID &&
                            tweet.EndTime = lastQueriedTime &&
                            tweet.TweetFields = toParams tweetFields &&
                            tweet.UserFields = toParams userFields &&
                            tweet.Expansions = toParams expansions &&
                            tweet.PollFields = toParams pollFields &&
                            Option.forall (fun token -> token = tweet.PaginationToken) paginationToken
                            )
                        select tweet
                }

                this.makeTwitterSingleQuery $"Problem getting mentions after the last queried time {lastQueriedTime.ToLongTimeString()}" query

            member this.GetTweets(ids: string seq) =
                let query() = query {
                    for tweet in context.Tweets do
                        where (
                            tweet.Type = TweetType.Lookup &&
                            tweet.Ids = String.concat "," ids &&
                            tweet.TweetFields = toParams tweetFields &&
                            tweet.UserFields = toParams userFields &&
                            tweet.Expansions = toParams expansions &&
                            tweet.PollFields = toParams pollFields
                        )
                        select tweet
                }

                this.makeTwitterSingleQuery $"Problem getting tweets with ids {ids}" query 

            member this.GetUser(userID:string) =
                let query () = query {
                    for user in context.TwitterUser do
                        where ( 
                            user.Type = UserType.IdLookup && 
                            user.ID = userID &&
                            user.UserFields = toParams userFields
                            )
                        select user
                }

                this.makeTwitterSingleQuery $"Problem getting user with ID {userID}" query

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

            member this.getTweetMediaEntities(tweetIDs: string seq) =
                let ids = String.concat "," tweetIDs

                let query () = query {
                    for tweet in context.Status do
                        where (tweet.Type = StatusType.Mentions
                            && tweet.TweetIDs = ids
                        )
                        select (tweet.ID, tweet.ExtendedEntities.MediaEntities |> Seq.toList)
                    }

                this.makeTwitterListQuery $"Problem trying to retrieve extended entities for {tweetIDs}" query



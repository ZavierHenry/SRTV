namespace SRTV

module TweetMedia =
    open System
    open LinqToTwitter

    open Twitter
    open System.Text.RegularExpressions
    open Substitution

    open Utilities
    open Utilities.DateTimePatterns

    open Humanizer

    type SpeakMode = | Timeline | Expanded

    type Media =
        | Image of altText : string option
        | Video of attribution : string option
        | Gif of altText : string option
        | Card of title : string * desc : string * host : string
        | Poll of options : (string*int) list * endDateTime : DateTime

    let toTimeDeltaText (datetime:DateTime) =
        let now = DateTime.UtcNow
        match datetime.ToUniversalTime() with
        | BeforeThisYear now _ as d -> d.ToString("dd MMM yy")
        | BeforeThisWeek now _ as d -> d.ToString("dd MMM")
        | DaysAgo now days when days > 0 ->
            "day".ToQuantity(days) + " ago"
        | MinutesAgo now min when min > 0 ->
            "minute".ToQuantity(min) + " ago"
        | SecondsAgo now sec ->
            "second".ToQuantity(sec) + " ago"
        | datetime -> datetime.ToLongTimeString()
    
    let endDateTimeToText (endDate: DateTime) =
        let now = DateTime.UtcNow
        match endDate.ToUniversalTime() with
        | SecondsFromNow now sec when sec < 60 -> 
            "second".ToQuantity(sec) + " left"
        | MinutesFromNow now min when min < 60 -> 
            "minute".ToQuantity(min) + " left"
        | HoursFromNow now hrs -> 
            "hour".ToQuantity(hrs) + " left"
        | DaysFromNow now days ->
            "day".ToQuantity(days) + " left"
        | date               -> 
            let days = int (date-now).TotalDays
            "day".ToQuantity(days) + " left"

    let mediaToText = function
        | Image alt         -> Option.defaultValue "image" alt
        | Video None        -> "embedded video"
        | Video (Some attribution) -> 
            $"embedded video attributed to %s{attribution}"
        | Gif text -> 
            let text = Option.defaultValue "embedded video" text
            $"%s{text} gif"
        | Card  (title, desc, host) -> 
            $"%s{title} %s{desc} %s{host}"
        | Poll  (options, endTime) when endTime > DateTime.UtcNow ->
            let endDateText = endDateTimeToText endTime
            let votes = List.sumBy (fun (_, votes) -> votes) options
            let choices = 
                List.map fst options
                |> String.concat " " 
            $"""%s{choices} %s{"vote".ToQuantity(votes)} %s{endDateText}"""
        | Poll (options, _) ->
            let total = List.sumBy (fun (_, votes) -> votes) options
            let choices =
                options
                |> List.map (fun (choice, votes) -> $"%s{choice} %.1f{float votes / float total * 100.0}%%")
                |> String.concat " "
            $"""%s{choices} %s{"vote".ToQuantity(total)} final results"""

    let repliesToString = function
        | []                -> ""
        | [ a ]             -> $"Replying to @{a}"
        | [ a; b]           -> $"Replying to @{a} and @{b}"
        | [ a; b; c ]       -> $"Replying to @{a} @{b} and @{c}"
        | a :: b :: rest    -> $"Replying to @{a} @{b} and {rest.Length} others"
        
    type QuotedTweet =
       | Unavailable
       | Tweet of
            screenName : string *
            name : string *
            verified : bool *
            locked: bool *
            date : DateTime *
            repliedTo : string list *
            text : string *
            media : Media list *
            hasPoll : bool

    let quotedTweetToString = function
    | Unavailable -> ""
    | Tweet (screenName, name, verified, locked, date, repliedTo, text, media, hasPoll) ->
        sprintf "quote tweet %s%s %s%s%s%s%s%s"
            <| repliesToString repliedTo
            <| name
            <| if verified then " verified account " else ""
            <| if locked then " protected account " else " "
            <| $"@{screenName}"
            <| toTimeDeltaText date
            <| text
            <| if hasPoll then "Show this poll" else List.map mediaToText media |> String.concat ""

    let twitterTweetToQuotedTweet includes extendedEntities (tweet:Tweet) =
        let author = findUserById tweet.AuthorID includes

        Tweet (
            author.Username, 
            author.Name, 
            author.Verified,
            author.Protected,
            tweet.CreatedAt.Value, 
            [], 
            tweet.Text, 
            [], 
            Seq.isEmpty tweet.Attachments.PollIds |> not
        )

    type MockTweet(text, screenname, name, date, isVerified, isProtected, retweeter, repliedTo, quotedTweet, media) =
        member this.Text : string = text
        member this.ScreenName : string = screenname
        member this.Name : string = name
        member this.IsProtected : bool = isProtected
        member this.IsVerified : bool = isVerified
        member this.Date : DateTime = date
        member this.Retweeter : string option = retweeter
        member this.RepliedTo : string list = repliedTo
        member this.QuotedTweet : QuotedTweet option = quotedTweet
        member this.Media : Media seq = media

        new(tweet: Tweet, includes: Common.TwitterInclude, extendedEntities: Common.Entities.MediaEntity seq) =

            let originalTweet = 
                tryFindTweetReferenceByType "retweeted" tweet.ReferencedTweets 
                |> Option.map (fun ref -> findTweetById ref.ID includes)
                |> Option.defaultValue tweet

            let quotedTweet =
                tryFindTweetReferenceByType "quoted" tweet.ReferencedTweets
                |> Option.map (fun ref -> findTweetById ref.ID includes)
                |> Option.map (twitterTweetToQuotedTweet includes extendedEntities)

            let author = findUserById originalTweet.AuthorID includes

            let repliedTo = 
                tryFindTweetReferenceByType "replied_to" tweet.ReferencedTweets
                |> Option.bind (fun ref -> tryFindTweetById ref.ID includes)
                |> Option.bind (fun tweet -> tryFindUserById tweet.AuthorID includes)
                |> Option.map (fun user -> user.Username)
                |> Option.toList

            let retweeter =
                if originalTweet.ID <> tweet.ID
                then tryFindUserById tweet.ID includes |> Option.map (fun user -> user.Name)
                else None

            let (media:TweetMedia seq) = 
                Seq.filter (fun m -> Seq.contains m.MediaKey tweet.Attachments.MediaKeys) includes.Media

            let photos = 
                let extendedPhotoEntities = extendedEntities |> Seq.filter (fun x -> x.Type = "photo")
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.Photo)
                |> Seq.map (fun x -> extendedPhotoEntities |> Seq.find (fun y -> x.PreviewImageUrl = y.MediaUrlHttps))
                |> Seq.map (fun x -> Image <| tryNonBlankString x.AltText)

            let videos = 
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.Video)
                |> Seq.map (fun _ -> Video None)

            let gifs = 
                let extendedGifEntities = extendedEntities |> Seq.filter (fun x -> x.Type = "animated_gif")
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.AnimatedGif)
                |> Seq.map (fun x -> extendedGifEntities |> Seq.find (fun y -> x.PreviewImageUrl = y.MediaUrlHttps))
                |> Seq.map (fun x -> Gif <| tryNonBlankString x.AltText)

            let polls =
                includes.Polls
                |> Seq.filter (fun poll -> Seq.contains poll.ID tweet.Attachments.PollIds)
                |> Seq.map (fun poll -> Poll (poll.Options |> Seq.map (fun opt -> (opt.Label, opt.Votes)) |> Seq.toList, poll.EndDatetime))

            let card = 
                tweet.Entities.Urls
                |> Seq.filter (fun (url:TweetEntityUrl) -> Seq.isEmpty url.Images |> not)
                |> Seq.map (fun (url:TweetEntityUrl) -> 
                    let host = Uri(url.UnwoundUrl).Host
                    let host = Regex.Match(host, "(?:www\.)?(.*?)").Groups.[1].Value
                    Card (url.Title, url.Description, host))

            MockTweet(
                originalTweet.Text,
                author.Username,
                author.Name,
                tweet.CreatedAt.Value,
                author.Verified,
                author.Protected,
                retweeter,
                repliedTo,
                quotedTweet,
                seq {
                    yield! photos
                    yield! videos
                    yield! gifs
                    yield! card
                    yield! polls
                }
            )

        member this.ToUnprocessedText() : string = 
            sprintf "%s %s %s %s @%s %s %s %s %s"
            <| match this.Retweeter with | Some name -> $"{name} retweeted " | None -> ""
            <| this.Name
            <| if this.IsVerified then " verified account " else " "
            <| if this.IsProtected then " protected account " else " "
            <| this.ScreenName
            <| repliesToString repliedTo
            <| removeBeginningReplies this.Text this.RepliedTo
            <| (if Seq.isEmpty this.Media then "" else " ") + String.concat " " (Seq.map mediaToText this.Media)
            <| (this.QuotedTweet |> Option.map quotedTweetToString |> Option.defaultValue "")

        member this.ToSpeakText() = processSpeakText <| this.ToUnprocessedText()
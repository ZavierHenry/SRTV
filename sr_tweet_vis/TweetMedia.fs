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

    type UrlType = | Media | QuoteTweet | Regular
    type Url = Url of url: string * displayUrl: string * urlType: UrlType

    let processLinks links text =
        let processLinks' text (Url (url, displayUrl, urlType)) =
            let newUrl = match urlType with | Regular -> displayUrl | _ -> ""
            Regex.Replace(text, Regex.Escape url, newUrl)
        Seq.fold processLinks' text links

    type Media =
        | Image of altText : string option
        | Video of attribution : string option
        | Gif of altText : string option
        | Card of url: string * title : string * desc : string * host : string
        | Poll of options : (string*int) list * endDateTime : DateTime

    let toTimeDeltaText (ref:DateTime) (datetime:DateTime) =
        match datetime.ToUniversalTime() with
        | BeforeThisYear ref _ as d -> d.ToString("MMM dd yy")
        | BeforeThisWeek ref _ as d -> d.ToString("MMM dd")
        | DaysAgo ref days when days > 0 ->
            "day".ToQuantity(days) + " ago"
        | HoursAgo ref hrs when hrs > 0 ->
            "hour".ToQuantity(hrs) + " ago"
        | MinutesAgo ref min when min > 0 ->
            "minute".ToQuantity(min) + " ago"
        | SecondsAgo ref sec ->
            "second".ToQuantity(sec) + " ago"
        | datetime -> datetime.ToLongTimeString()
    
    let endDateTimeToText (ref:DateTime) (endDate: DateTime) =
        match endDate.ToUniversalTime() with
        | SecondsFromNow ref sec when sec < 60 -> 
            "second".ToQuantity(sec) + " left"
        | MinutesFromNow ref min when min < 60 -> 
            "minute".ToQuantity(min) + " left"
        | HoursFromNow ref hrs when hrs < 23 -> 
            "hour".ToQuantity(hrs) + " left"
        | DaysFromNow ref days ->
            "day".ToQuantity(days) + " left"
        | date               -> 
            let days = int (date-ref).TotalDays
            "day".ToQuantity(days) + " left"

    let mediaToText (ref:DateTime) = function
        | Image alt         -> Option.defaultValue "image" alt
        | Video None        -> "embedded video"
        | Video (Some attribution) -> 
            $"embedded video attributed to %s{attribution}"
        | Gif text -> 
            let text = Option.defaultValue "embedded video" text
            $"%s{text} gif"
        | Card  (_, title, desc, host) -> 
            $"%s{title} %s{desc} %s{host}"
        | Poll  (options, endTime) when endTime > ref ->
            let endDateText = endDateTimeToText ref endTime
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
            urls : Url seq *
            hasPoll : bool

    let quotedTweetToString ref = function
    | Unavailable -> ""
    | Tweet (screenName, name, verified, locked, date, repliedTo, text, media, urls, hasPoll) ->
        sprintf "quote tweet %s %s %s %s @%s %s %s %s"
            <| repliesToString repliedTo
            <| name
            <| if verified then "verified account" else ""
            <| if locked then "protected account" else ""
            <| screenName
            <| toTimeDeltaText ref date
            <| (removeBeginningReplies text repliedTo |> processLinks urls)
            <| if hasPoll then "show this poll" else List.map (mediaToText ref) media |> String.concat ""

    let twitterTweetToQuotedTweet includes extendedEntities (tweet:Tweet) =
        let author = findUserById tweet.AuthorID includes

        //let urls =
        //    match tweet.Entities with
        //    | null -> Seq.empty
        //    | entities ->
        //        match entities.Urls with
        //        | null -> Seq.empty
        //        | urls -> Seq.cast<TweetEntityUrl> urls
        //    |> Seq.sortBy (fun url -> url.Start)

        let urls =
            Option.ofObj tweet.Entities
            |> Option.map (fun entities -> nullableSequenceToValue entities.Urls)
            |> Option.defaultValue Seq.empty
            |> Seq.sortBy (fun url -> url.Start)

        let mockUrls =
            urls
            |> Seq.map (function 
                | url when url.Url <> (Seq.last urls).Url -> Url (url.Url, url.DisplayUrl, Regular)
                | url when Regex.IsMatch(url.ExpandedUrl, @"^https://twitter\.com/\w+/status/\d+/(photo|video))") ->
                    Url (url.Url, url.DisplayUrl, Media)
                | url -> Url (url.Url, url.DisplayUrl, Regular) )

        Tweet (
            author.Username, 
            author.Name, 
            author.Verified,
            author.Protected,
            tweet.CreatedAt.Value, 
            [], 
            tweet.Text, 
            [], 
            mockUrls,
            Option.ofObj tweet.Attachments 
            |> Option.map (fun x -> nullableSequenceToValue x.PollIds)
            |> Option.exists (not << Seq.isEmpty)
        )

    type MockTweet(text, screenname, name, date, isVerified, isProtected, retweeter, repliedTo, quotedTweet, media, urls) =
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
        member this.Urls : Url seq = urls

        new(tweet: Tweet, includes: Common.TwitterInclude, extendedEntities: Common.Entities.MediaEntity seq) =

            //let referencedTweets = match tweet.ReferencedTweets with | null -> Seq.empty | refs -> seq { yield! refs }
            let referencedTweets = nullableSequenceToValue tweet.ReferencedTweets

            let originalTweet = 
                tryFindTweetReferenceByType "retweeted" referencedTweets
                |> Option.map (fun ref -> findTweetById ref.ID includes)
                |> Option.defaultValue tweet

            let author = findUserById originalTweet.AuthorID includes

            let quotedTweet =
                tryFindTweetReferenceByType "quoted" referencedTweets
                |> Option.map (fun ref -> findTweetById ref.ID includes)
                |> Option.map (twitterTweetToQuotedTweet includes extendedEntities)

            let repliedTo = 
                tryFindTweetReferenceByType "replied_to" referencedTweets
                |> Option.bind (fun ref -> tryFindTweetById ref.ID includes)
                |> Option.bind (fun tweet -> tryFindUserById tweet.AuthorID includes)
                |> Option.map (fun user -> user.Username)
                |> Option.toList
                
            let retweeter =
                if originalTweet.ID <> tweet.ID
                then tryFindUserById tweet.ID includes |> Option.map (fun user -> user.Name)
                else None

            let (media:TweetMedia seq) =
                //match includes.Media with | null -> Seq.empty | media -> seq { yield! media }
                //|> Seq.filter (fun m -> Seq.contains m.MediaKey tweet.Attachments.MediaKeys)
                nullableSequenceToValue includes.Media
                |> Seq.filter (fun m -> 
                    Option.ofObj tweet.Attachments
                    |> Option.map (fun attachments -> nullableSequenceToValue attachments.MediaKeys)
                    |> Option.defaultValue Seq.empty
                    |> Seq.contains (Option.ofObj m.MediaKey |> Option.defaultValue ""))

            let photos = 
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.Photo)
                |> Seq.map (fun x -> 
                    Option.ofObj x.AltText
                    |> Option.map (tryNonBlankString >> Image)
                    |> Option.defaultValue (Image None))

            let videos = 
                let extendedVideoEntities = extendedEntities |> Seq.filter (fun x -> x.Type = "video")
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.Video)
                |> Seq.map (fun x -> extendedVideoEntities |> Seq.find (fun y -> string y.ID = Regex.Match(x.MediaKey, @"_(\d+)$").Groups.[1].Value))
                |> Seq.map (fun x ->
                    Option.ofObj x.SourceUser
                    |> Option.map (fun sourceUser -> Video <| tryNonBlankString sourceUser.Name)
                    |> Option.defaultValue (Video None))
                //|> Seq.map (fun x -> Video <| tryNonBlankString x.SourceUser.Name)

            let gifs = 
                let extendedGifEntities = extendedEntities |> Seq.filter (fun x -> x.Type = "animated_gif")
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.AnimatedGif)
                |> Seq.map (fun x -> extendedGifEntities |> Seq.find (fun y -> x.PreviewImageUrl = y.MediaUrlHttps))
                |> Seq.map (fun x ->
                    Option.ofObj x.AltText
                    |> Option.map (tryNonBlankString >> Gif)
                    |> Option.defaultValue (Gif None))
                //|> Seq.map (fun x -> Gif <| tryNonBlankString x.AltText)

            let polls =
                //match includes.Polls with | null -> Seq.empty | polls -> seq { yield! polls }
                nullableSequenceToValue includes.Polls
                |> Seq.filter (fun poll -> Seq.contains poll.ID tweet.Attachments.PollIds)
                |> Seq.map (fun poll -> Poll (poll.Options |> Seq.map (fun opt -> (opt.Label, opt.Votes)) |> Seq.toList, poll.EndDatetime))
                

            //let urls =
            //    match tweet.Entities with 
            //    | null -> Seq.empty 
            //    | entities -> 
            //        match entities.Urls with 
            //        | null -> Seq.empty 
            //        | urls -> Seq.cast<TweetEntityUrl> urls
            //    |> Seq.sortBy (fun url -> url.Start)

            let urls =
                Option.ofObj tweet.Entities
                |> Option.map (fun entities -> nullableSequenceToValue entities.Urls)
                |> Option.defaultValue Seq.empty
                |> Seq.sortBy (fun url -> url.Start)

            let cards = 
                urls
                |> Seq.filter (fun (url:TweetEntityUrl) -> nullableSequenceToValue url.Images |> Seq.isEmpty |> not)
                |> Seq.map (fun (url:TweetEntityUrl) -> 
                    let host = Uri(Option.ofObj url.UnwoundUrl |> Option.defaultValue "").Host
                    let host = Regex.Match(host, "(?:www\.)?(.*?)").Groups.[1].Value
                    Card (url.Url, url.Title, url.Description, host))

            let mockUrls =
                urls
                |> Seq.map (function 
                    | url when url.Url <> (Seq.last urls).Url -> Url (url.Url, url.DisplayUrl, Regular)
                    | url when Seq.exists (function | Card (u, _, _, _) -> u = url.Url | _ -> false) cards ->
                        Url (url.Url, url.DisplayUrl, Media)
                    | url when 
                        tryFindTweetReferenceByType "quoted" referencedTweets
                        |> Option.exists (fun ref -> 
                            Regex.Match(url.ExpandedUrl, @"^https://twitter\.com/\w+/status/(?<id>\d+)$").Groups.["id"].Value = ref.ID) ->
                        Url (url.Url, url.DisplayUrl, QuoteTweet)
                    | url when Regex.IsMatch(url.ExpandedUrl, @"^https://twitter\.com/\w+/status/\d+/(photo|video)") ->
                        Url (url.Url, url.DisplayUrl, Media)
                    | url -> Url (url.Url, url.DisplayUrl, Regular) )

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
                Seq.concat <| seq { photos; videos; gifs; cards; polls },
                mockUrls
            )

        member this.ToUnprocessedText(ref: DateTime) : string = 
            sprintf "%s %s"
            <| (removeBeginningReplies this.Text this.RepliedTo |> processLinks this.Urls)
            <| (if Seq.isEmpty this.Media then "" else " ") + String.concat " " (Seq.map (mediaToText ref) this.Media)

        member this.ToFullUnprocessedText(ref: DateTime) : string = 
            sprintf "%s %s %s %s @%s %s %s %s %s %s"
            <| match this.Retweeter with | Some name -> $"{name} retweeted " | None -> ""
            <| this.Name
            <| if this.IsVerified then " verified account " else " "
            <| if this.IsProtected then " protected account " else " "
            <| this.ScreenName
            <| toTimeDeltaText ref this.Date
            <| repliesToString repliedTo
            <| (removeBeginningReplies this.Text this.RepliedTo |> processLinks this.Urls)
            <| (this.QuotedTweet |> Option.map (quotedTweetToString ref) |> Option.defaultValue "")
            <| (if Seq.isEmpty this.Media then "" else " ") + String.concat " " (Seq.map (mediaToText ref) this.Media)
    
        member this.ToSpeakText(?ref: DateTime) : string = 
            Option.defaultValue DateTime.UtcNow ref
            |> this.ToUnprocessedText
            |> processSpeakText

        member this.ToFullSpeakText(?ref: DateTime) : string =
            Option.defaultValue DateTime.UtcNow ref
            |> this.ToFullUnprocessedText
            |> processSpeakText




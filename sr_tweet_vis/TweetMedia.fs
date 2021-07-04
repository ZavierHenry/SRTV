namespace SRTV

module TweetMedia =
    open System
    open LinqToTwitter
    open TwitterClient
    open System.Text.RegularExpressions

    let pluralize prefix (number:int) =
        prefix + if number = 1 then "s" else ""

    type SpeakMode = | Timeline | Expanded

    type Media =
        | Image of altText : string option
        | Video of attribution : string option
        | Gif of altText : string option
        | Card of title : string * desc : string * host : string
        | Poll of options : (string*int) list * endDateTime : DateTime

    let toTimeDeltaText (datetime:DateTime) =
    
        let datetime = datetime.ToUniversalTime()
        let now = DateTime.UtcNow
        let oneWeekAgo = now.AddDays(-7.0)
        let timespan = now - datetime
    
        if now.Year - datetime.Year > 0 then datetime.ToString("dd MMM yy")
        else if now < oneWeekAgo then datetime.ToString("dd MMM")
        else if timespan.Days > 0 then $"""%d{timespan.Days} %s{if timespan.Days = 1 then "days" else "day"} ago"""
        else if timespan.Minutes > 0 then $"""%d{timespan.Minutes} %s{if timespan.Minutes = 1 then "minutes" else "minute"} ago"""
        else $"""%d{timespan.Seconds} %s{if timespan.Seconds = 1 then "seconds" else "second" } ago"""

    let endDateTimeToText (endDate: DateTime) =
        let now = DateTime.UtcNow
        let (|SecondsLeft|_|) (timespan:TimeSpan) = 
            if timespan.TotalSeconds < 60.0 then Some timespan.Seconds else None
        let (|MinutesLeft|_|) (timespan:TimeSpan) =
            if timespan.TotalMinutes < 60.0 then Some timespan.Minutes else None
        let (|HoursLeft|_|) (timespan:TimeSpan) =
            if timespan.TotalHours < 24.0 then Some timespan.Hours else None

        let addPlural num = if num = 1 then "s" else ""

        match (endDate - now) with
        | SecondsLeft sec -> $"""%d{sec} second%s{addPlural sec}"""
        | MinutesLeft min -> $"""%d{min} minute%s{addPlural min}"""
        | HoursLeft hrs   -> $"""%d{hrs} hour%s{addPlural hrs}"""
        | x               -> let days = x.Days in $"""%d{days} day%s{addPlural days}"""

    let mediaToText = function
        | Image alt         -> Option.defaultValue "image" alt
        | Video None        -> "embedded video"
        | Video (Some attribution) -> 
            $"embedded video attributed to %s{attribution}"
        | Gif   None        -> "embedded video gif"
        | Gif   (Some text) -> $"%s{text} gif"
        | Card  (title, desc, host) -> 
            $"%s{title} %s{desc} %s{host}"
        | Poll  (options, endTime) when endTime > DateTime.UtcNow ->
            let endDateText = endDateTimeToText endTime
            let votes = List.sumBy (fun (_, votes) -> votes) options
            let choices = 
                List.map fst options
                |> String.concat " " 
            $"""%s{choices} %d{votes} %s{pluralize "vote" votes} %s{endDateText} left"""
        | Poll (options, _) ->
            let total = List.sumBy snd options
            let choices =
                List.map (fun (choice, votes) -> $"%s{choice} %.1f{float votes / float total * 100.0}%%") options
                |> String.concat " "
            $"""%s{choices} %d{total} %s{pluralize "vote" total} Final results"""

    let wrapStringIfNotBlank str = Some str |> Option.filter (String.IsNullOrEmpty >> not)

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
        sprintf "%s%s %s%s%s%s%s%s"
            <| repliesToString repliedTo
            <| name
            <| if verified then " verified account " else ""
            <| if locked then " protected account " else " "
            <| $"@{screenName}"
            <| toTimeDeltaText date
            <| text
            <| if hasPoll then "Show this poll" else List.map mediaToText media |> String.concat ""

    let twitterTweetToQuotedTweet (tweet:Tweet) includes extendedEntities =
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

    type MockTweet(text, screenname, name, date, isVerified, isProtected, retweeter, repliedTo, media) =
        member this.Text : string = text
        member this.ScreenName : string = screenname
        member this.Name : string = name
        member this.IsProtected : bool = isProtected
        member this.IsVerified : bool = isVerified
        member this.Date : DateTime = date
        member this.Retweeter : string option = retweeter
        member this.RepliedTo : string list = repliedTo
        member this.QuotedTweet : QuotedTweet option = None
        member this.Media : Media list = media

        new(tweet: Tweet, includes: Common.TwitterInclude, extendedEntities: Common.Entities.MediaEntity seq) =

            let originalTweet = 
                match tryFindTweetReferenceByType "retweeted" tweet.ReferencedTweets with
                | None      -> tweet
                | Some ref  -> findTweetById ref.ID includes

            let author = findUserById originalTweet.AuthorID includes

            let repliedTo = 
                match tryFindUserById originalTweet.InReplyToUserID includes with
                | None -> []
                | Some user -> [user.Username]

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
                |> Seq.map (fun x -> wrapStringIfNotBlank x.AltText |> Image)
                |> Seq.toList

            let videos = 
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.Video)
                |> Seq.map (fun _ -> Video None)
                |> Seq.toList

            let gifs = 
                let extendedGifEntities = extendedEntities |> Seq.filter (fun x -> x.Type = "animated_gif")
                media
                |> Seq.filter (fun x -> x.Type = TweetMediaType.AnimatedGif)
                |> Seq.map (fun x -> extendedGifEntities |> Seq.find (fun y -> x.PreviewImageUrl = y.MediaUrlHttps))
                |> Seq.map (fun x -> wrapStringIfNotBlank x.AltText |> Gif)
                |> Seq.toList

            let poll = []

            let card = 
                tweet.Entities.Urls
                |> Seq.filter (fun (url:TweetEntityUrl) -> Seq.isEmpty url.Images |> not)
                |> Seq.map (fun (url:TweetEntityUrl) -> 
                    let host = Uri(url.UnwoundUrl).Host
                    let host = Regex.Match(host, "(?:www\.)?(.*?)").Groups.[1].Value
                    Card (url.Title, url.Description, host))
                |> Seq.toList

            MockTweet(
                originalTweet.Text,
                author.Username,
                author.Name,
                tweet.CreatedAt.Value,
                author.Verified,
                author.Protected,
                retweeter,
                repliedTo,
                photos @ videos @ gifs @ card @ poll
            )
    
        member this.ToSpeakText() : string = 
            sprintf "%s%s%s%s@%s%s %s%s"
            <| match this.Retweeter with | Some name -> $"Retweeted by @%s{name} " | None -> ""
            <| this.Name
            <| if this.IsVerified then " verified account " else " "
            <| if this.IsProtected then " protected account " else " "
            <| this.ScreenName
            <| repliesToString repliedTo
            <| this.Text 
            <| (if this.Media.IsEmpty then "" else " ") + String.concat " " (List.map mediaToText this.Media)


    module Text =

        let private transformSymbol = function
        | '_' -> " underscore "
        | '/' -> " slash "
        | '%' -> " percent "
        | character -> $"%c{character}"


        let unusedCharacters = "()"


        let countUrls (text:string) = 0

        let replace (before:string) (after:string) (text:string) =
            text.Replace(before, after)

        let letterizeUsedPunctuation = String.collect transformSymbol
        

        let letterizeCurrency (text:string) = ""
        
        let letterizeNumbers (text:string) = ""

        let letterizeEmojis (text:string) = ""

        let letterizeDate (text:string) = ""

        let toSpeechText = letterizeCurrency >> letterizeNumbers >> letterizeEmojis
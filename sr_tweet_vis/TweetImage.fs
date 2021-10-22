namespace SRTV

module TweetImage =
    open PuppeteerSharp
    open PuppeteerSharp.Media

    open FSharp.Data
    open FSharp.Data.HtmlActivePatterns

    open TweetMedia
    open Utilities

    module Doc = FSharp.Data.HtmlDocument
    module Node = FSharp.Data.HtmlNode

    let [<Literal>] ChromeExecutableKey = "CHROME_EXECUTABLE"

    let toAssocList = Seq.map (function | HtmlAttribute x -> x)
    let setText text = function
        | HtmlElement (tag, attrs, children) ->
            let elements = 
                seq {
                    yield! Seq.filter (function | HtmlText _ -> false | _ -> true) children
                    HtmlNode.NewText text
                }
            HtmlNode.NewElement (tag, toAssocList attrs, elements)
        | node -> node
    
    let setAttribute name value = function
        | HtmlElement (tag, attrs, children) ->
            let attrs = Seq.map (function | HtmlAttribute (n, v) -> (n, if n = name then value else v)) attrs
            HtmlNode.NewElement (tag, attrs, children)
        | node -> node

    let rec transformNode cond transformation (node:HtmlNode) =
        match cond node with
        | true -> transformation node
        | false ->
            match node with
            | HtmlElement (tag, attrs, children) ->
                HtmlNode.NewElement (tag, toAssocList attrs, Seq.map (transformNode cond transformation) children)
            | node -> node

    let transformDOM cond transformation (document:HtmlDocument) =
        Doc.elements document
        |> List.map (transformNode cond transformation)
        |> HtmlDocument.New

    type ImageTemplate = HtmlProvider<"assets/template.html">

    type ImageTweetInfo = {
        name: string
        screenName: string
        profileUrl: string
        verified: bool
        locked: bool

        source: string
        date: System.DateTime
        text: string
    }


    let tweetInfoToImage (tweetInfo:ImageTweetInfo) theme =
            
        //let text = 
        //    match renderOptions with
        //    | Image (_, Version.Full) | Text Version.Full | Video Version.Full ->
        //        mockTweet.ToFullSpeakText(ref)
        //    | Image (_, Version.Regular) | Text Version.Regular | Video Version.Regular ->
        //        mockTweet.ToSpeakText(ref)

        let document =
            ImageTemplate.GetSample().Html
            |> transformDOM (Node.hasId "tweetContainer") (Theme.toAttributeValue theme |> setAttribute "theme")
            |> transformDOM (Node.hasId "pfp") (setAttribute "src" tweetInfo.profileUrl)
            |> transformDOM (Node.hasId "username") (setText  $"@{tweetInfo.screenName}")
            |> transformDOM (Node.hasId "name") (setText tweetInfo.name)
            |> transformDOM (Node.hasId "verified") (setAttribute "style" (if tweetInfo.verified then "" else "display:none;"))
            |> transformDOM (Node.hasId "protected") (setAttribute "style" (if tweetInfo.locked then "" else "display:none;"))
            |> transformDOM (Node.hasId "dateOutput") (setText <| tweetInfo.date.ToString(@"MMM d, yyyy"))
            |> transformDOM (Node.hasId "timeOutput") (setText <| tweetInfo.date.ToString(@"h\:mm tt"))
            |> transformDOM (Node.hasId "client") (setText tweetInfo.source)
            |> transformDOM (Node.hasId "tweetText") (setText tweetInfo.text)

        async {

            do! BrowserFetcher().DownloadAsync(BrowserFetcher.DefaultChromiumRevision) |> Async.AwaitTask |> Async.Ignore
            let chromePath = tryFindEnvironmentVariable ChromeExecutableKey |> Option.defaultValue ""

            let launchOptions = LaunchOptions(Headless = true, Args = [| "--no-sandbox" |], ExecutablePath = chromePath)

            //launchOptions.Headless <- true

            let! browser = Puppeteer.LaunchAsync(launchOptions) |> Async.AwaitTask
            let! page = browser.NewPageAsync() |> Async.AwaitTask

            do! document.ToString() |> page.SetContentAsync |> Async.AwaitTask

            let! width = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetWidth") |> Async.AwaitTask
            let! height = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetHeight") |> Async.AwaitTask
            let! left = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetLeft") |> Async.AwaitTask
            let! top = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetTop") |> Async.AwaitTask

            let clip = Clip(Width = width, Height = height, X = left, Y = top)
            //clip.Width <- width
            //clip.Height <- height
            //clip.X <- left
            //clip.Y <- top

            let screenshotOptions = ScreenshotOptions(Clip = clip)
            //screenshotOptions.Clip <- clip

            return! page.ScreenshotDataAsync(screenshotOptions) |> Async.AwaitTask
        }

    let toImage (mockTweet:MockTweet) profileUrl source ref renderOptions =
        let tweetInfo = {
            name = mockTweet.Name
            screenName = mockTweet.ScreenName
            profileUrl = profileUrl
            source = source
            verified = mockTweet.IsVerified
            locked = mockTweet.IsProtected

            date = mockTweet.Date
            text =
                match renderOptions with
                | Image (_, Version.Full) | Text Version.Full | Video Version.Full ->
                    mockTweet.ToFullSpeakText(ref)
                | Image (_, Version.Regular) | Text Version.Regular | Video Version.Regular ->
                    mockTweet.ToSpeakText(ref)
        }

        tweetInfoToImage tweetInfo (match renderOptions with | Image (theme, _) -> theme | Text _ | Video _ -> Theme.Dim)


        
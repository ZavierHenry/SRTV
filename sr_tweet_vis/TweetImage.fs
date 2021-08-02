namespace SRTV

module TweetImage =
    open PuppeteerSharp
    open PuppeteerSharp.Media

    open FSharp.Data
    open FSharp.Data.HtmlActivePatterns

    open TweetMedia

    module Doc = FSharp.Data.HtmlDocument
    module Node = FSharp.Data.HtmlNode

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

    type Theme = 
        Light | Dim | Dark
        static member toAttributeValue = function
            | Light -> "light"
            | Dim -> "dim"
            | Dark -> "dark"

    let toImage (mockTweet:MockTweet) profileUrl source theme =
        let document =
            ImageTemplate.GetSample().Html
            |> transformDOM (Node.hasId "tweetContainer") (Theme.toAttributeValue theme |> setAttribute "theme")
            |> transformDOM (Node.hasId "pfp") (setAttribute "src" profileUrl)
            |> transformDOM (Node.hasId "username") (setText  $"@{mockTweet.ScreenName}")
            |> transformDOM (Node.hasId "name") (setText mockTweet.Name)
            |> transformDOM (Node.hasId "verified") (setAttribute "style" (if mockTweet.IsVerified then "" else "display:none;"))
            |> transformDOM (Node.hasId "protected") (setAttribute "style" (if mockTweet.IsProtected then "" else "display:none;"))
            |> transformDOM (Node.hasId "dateOutput") (setText <| mockTweet.Date.ToString(@"MMM d, yyyy"))
            |> transformDOM (Node.hasId "timeOutput") (setText <| mockTweet.Date.ToString(@"h\:mm tt"))
            |> transformDOM (Node.hasId "client") (setText source)
            |> transformDOM (Node.hasId "tweetText") (setText <| mockTweet.ToSpeakText())

        async {
            let! _ = BrowserFetcher().DownloadAsync(BrowserFetcher.DefaultChromiumRevision) |> Async.AwaitTask

            let launchOptions = LaunchOptions()
            launchOptions.Headless <- true

            let! browser = Puppeteer.LaunchAsync(launchOptions) |> Async.AwaitTask
            let! page = browser.NewPageAsync() |> Async.AwaitTask

            do! document.ToString() |> page.SetContentAsync |> Async.AwaitTask

            let! width = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetWidth") |> Async.AwaitTask
            let! height = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetHeight") |> Async.AwaitTask
            let! left = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetLeft") |> Async.AwaitTask
            let! top = page.EvaluateExpressionAsync<decimal>(@"document.getElementById(""tweetContainer"").offsetTop") |> Async.AwaitTask

            let clip = Clip()
            clip.Width <- width
            clip.Height <- height
            clip.X <- left
            clip.Y <- top

            let screenshotOptions = ScreenshotOptions()
            screenshotOptions.Clip <- clip

            return! page.ScreenshotDataAsync(screenshotOptions) |> Async.AwaitTask
        }
// Learn more about F# at http://fsharp.org

open System
open SRTV.TweetMedia
open SRTV.TweetAudio

open System.IO
open System.Web

open SRTV.Utilities

open FSharp.Data
open FSharp.Data.HtmlActivePatterns

open PuppeteerSharp
open PuppeteerSharp.Media

let exampleMockTweet =
    MockTweet(
        "I love that the Harvey’s burger chain got its name because the John Harvey Motors car dealership was going out of business and a guy opening a burger shop got the dealership sign for cheap.  A thrifty Canadian icon.",
        //"Anne Thériault",
        "anne_theriault",
        "Anne Theriault",
        DateTime.Parse("2021-05-07T16:31:38.000Z").ToUniversalTime(),
        true,
        false,
        None,
        [],
        []
)

let synthesize imagefile outfile = async {
    use synth = new Synthesizer()
    do! synth.Synthesize(exampleMockTweet, imagefile, outfile)
}

let speak () =
    use synth = new Synthesizer()
    synth.speak(exampleMockTweet.ToSpeakText())

module Doc = FSharp.Data.HtmlDocument
module Node = FSharp.Data.HtmlNode

let toAssocList = Seq.map (function | HtmlAttribute x -> x)
let getAttributeSequence node = Node.attributes node |> toAssocList
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

let setImageSrc src = setAttribute "src" src

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

let toImage'(output:string) =
    let source = "Twitter for iPhone"
    let profileUrl = "https://pbs.twimg.com/profile_images/1011409104441630720/ksmEpPII_normal.jpg"

    let document =
        ImageTemplate.GetSample().Html
        |> transformDOM (Node.hasId "pfp") (setImageSrc profileUrl)
        |> transformDOM (Node.hasId "username") (setText  $"@{exampleMockTweet.ScreenName}")
        |> transformDOM (Node.hasId "name") (setText exampleMockTweet.Name)
        |> transformDOM (Node.hasId "verified") (setAttribute "style" (if exampleMockTweet.IsVerified then "" else "display:none;"))
        |> transformDOM (Node.hasId "protected") (setAttribute "style" (if exampleMockTweet.IsProtected then "" else "display:none;"))
        |> transformDOM (Node.hasId "monthOutput") (setText <| exampleMockTweet.Date.ToString("MMM "))
        |> transformDOM (Node.hasId "dayOutput") (setText <| exampleMockTweet.Date.Day.ToString())
        |> transformDOM (Node.hasId "yearOutput") (setText <| exampleMockTweet.Date.ToString("yyyy"))
        |> transformDOM (Node.hasId "timeOutput") (setText <| exampleMockTweet.Date.ToString(@"h\:mm tt"))
        |> transformDOM (Node.hasId "client") (setText source)
        |> transformDOM (Node.hasId "tweetText") (exampleMockTweet.ToSpeakText() |> HttpUtility.HtmlEncode |> setText)

    File.WriteAllText("results.html", document.ToString())

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

        do! page.ScreenshotAsync(output, screenshotOptions) |> Async.AwaitTask
    }
    

[<EntryPoint>]
let main argv =
    match argv with
    | [| imagefile; outputfile |]   -> synthesize imagefile outputfile |> Async.RunSynchronously
    | [| outfile |]                 -> toImage' outfile |> Async.RunSynchronously
    | _                             -> speak ()
    0
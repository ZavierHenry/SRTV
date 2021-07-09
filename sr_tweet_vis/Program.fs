// Learn more about F# at http://fsharp.org

open System
open SRTV.TweetMedia
open SRTV.TweetAudio

open System.IO
open System.Web

open SRTV.Utilities

open FSharp.Data
open FSharp.Data.HtmlActivePatterns

open CoreHtmlToImage


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

let setImageSrc src = function
    | HtmlElement (tag, attrs, children) ->
        let attrs = Seq.map (function | HtmlAttribute ("src", _) -> ("src", src) | HtmlAttribute x -> x) attrs
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

let transformDOM cond transformation (document:FSharp.Data.HtmlDocument) =
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
        |> transformDOM (Node.hasId "monthOutput") (setText <| exampleMockTweet.Date.ToString("MMM "))
        |> transformDOM (Node.hasId "dayOutput") (setText <| exampleMockTweet.Date.Day.ToString())
        |> transformDOM (Node.hasId "yearOutput") (setText <| exampleMockTweet.Date.ToString("yyyy"))
        |> transformDOM (Node.hasId "timeOutput") (setText <| exampleMockTweet.Date.ToString(@"h\:mm tt"))
        |> transformDOM (Node.hasId "client") (setText source)
        |> transformDOM (Node.hasId "tweetText") (exampleMockTweet.ToSpeakText() |> HttpUtility.HtmlEncode |> setText)

    File.WriteAllText("results.html", document.ToString())

    let bytes = CoreHtmlToImage.HtmlConverter().FromHtmlString(document.ToString(), 700, ImageFormat.Jpg, 100)
    File.WriteAllBytes(output, bytes)
    

[<EntryPoint>]
let main argv =
    match argv with
    | [| imagefile; outputfile |]   -> synthesize imagefile outputfile |> Async.RunSynchronously
    | [| outfile |]                 -> toImage' outfile
    | _                             -> speak ()
    0
// Learn more about F# at http://fsharp.org

open System
open SRTV.TweetMedia
open SRTV.TweetAudio

open Aspose.Html.Saving
open Aspose.Html.Rendering.Image
open Aspose.Html.Converters
open Aspose.Html

open System.IO

open SRTV.Utilities

    

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

let setElementTextById id text (document:HTMLDocument) =
    document.GetElementById(id).TextContent <- text
    document

let toImage (outputFile:string) =
    let template = "./Downloads/template.html"

    let source = "Twitter for iPhone"
    let profileUrl = "https://pbs.twimg.com/profile_images/1011409104441630720/ksmEpPII_normal.jpg"

    use document = new HTMLDocument(template)

    document
    |> setElementTextById "usernameOutput" exampleMockTweet.ScreenName
    |> setElementTextById "nameOutput" exampleMockTweet.Name
    |> setElementTextById "monthOutput" (exampleMockTweet.Date.ToString("MMM "))
    |> setElementTextById "dayOutput" (exampleMockTweet.Date.Day.ToString())
    |> setElementTextById "yearOutput" (exampleMockTweet.Date.ToString("yyyy"))
    |> setElementTextById "timeOutput" (exampleMockTweet.Date.ToString(@"h\:mm tt"))
    |> setElementTextById "clientOutput" source
    |> setElementTextById "tweetTextOutput" (exampleMockTweet.ToSpeakText())
    |> ignore

    let profileNode = document.CreateElement("img")
    profileNode.SetAttribute("src", profileUrl)

    document.GetElementById("profilePicture").AppendChild(profileNode) |> ignore
    let verifiedStyle = document.GetElementById("verifiedOutput")

    if exampleMockTweet.IsVerified
    then verifiedStyle.RemoveAttribute("style")
    else verifiedStyle.SetAttribute("style", "display:none;")

    let testHtmlOutput = "./Downloads/test_result.html"
    document.Save(testHtmlOutput)
    
    let options = ImageSaveOptions(ImageFormat.Jpeg)
    Converters.Converter.ConvertHTML(document, options, outputFile)
    

[<EntryPoint>]
let main argv =
    match argv with
    | [| imagefile; outputfile |]   -> synthesize imagefile outputfile |> Async.RunSynchronously
    | [| outfile |]                 -> toImage outfile
    | _                             -> speak ()
    0
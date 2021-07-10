// Learn more about F# at http://fsharp.org

open System

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

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
        "anne_theriault",
        "Anne Thériault",
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


let toImage'(output:string) =
    let source = "Twitter for iPhone"
    let profileUrl = "https://pbs.twimg.com/profile_images/1011409104441630720/ksmEpPII_normal.jpg"

    async {
        let! bytes = toImage exampleMockTweet profileUrl source
        return File.WriteAllBytes(output, bytes)
    }
    
    

[<EntryPoint>]
let main argv =
    match argv with
    | [| imagefile; outputfile |]   -> synthesize imagefile outputfile |> Async.RunSynchronously
    | [| outfile |]                 -> toImage' outfile |> Async.RunSynchronously
    | _                             -> speak ()
    0
// Learn more about F# at http://fsharp.org

open System

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

open SRTV.Twitter.TwitterClient
open SRTV.Twitter.Patterns

open System.IO

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
        let! bytes = toImage exampleMockTweet profileUrl source Theme.Dim
        return File.WriteAllBytes(output, bytes)
    }

let rec handleMentions (client:Client) startDate (token: string option) = async {

    let! mentions = client.GetMentions(startDate)
    let! extendedEntities =
        mentions
        |> ClientResult.map ( fun mentions -> mentions.Tweets |> Seq.filter (function | MediaTweet _ -> true | _ -> false) )
        |> ClientResult.map ( fun tweets -> tweets |> Seq.map (fun tweet -> tweet.ID) )
        |> bindAsync client.getTweetMediaEntities

    //TODO: convert to SRTV tweet and send

    do! match mentions with
        | Success mentions ->
            match mentions.Meta.NextToken with
            | "" -> async { return () }
            | token -> handleMentions client startDate (Some token)
        | _ -> async { return () }

}
    
[<EntryPoint>]
let main argv =
    let client = Client()

    let startDate = 
        match argv with
        | [| |]     -> DateTime.UtcNow.AddMonths(-1)
        | argv      -> DateTime.Parse <| Array.head argv

    let mentions = handleMentions client startDate None |> Async.RunSynchronously

    0

    

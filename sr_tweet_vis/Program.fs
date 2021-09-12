// Learn more about F# at http://fsharp.org

open System
open System.IO

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

open SRTV.Twitter.TwitterClient
open SRTV.Twitter.Patterns
open SRTV.Twitter.Patterns.Mentions

open System.Reflection



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
    synth.Speak(exampleMockTweet.ToSpeakText(), "random_file.mp4")


let toImage'(output:string) =
    let source = "Twitter for iPhone"
    let profileUrl = "https://pbs.twimg.com/profile_images/1011409104441630720/ksmEpPII_normal.jpg"

    async {
        let! bytes = toImage exampleMockTweet profileUrl source Theme.Dim
        return File.WriteAllBytes(output, bytes)
    }

let rec handleMentions (client:Client) startDate (token: string option) = async {

    let! mentions = client.GetMentions(startDate)
    let tweets = 
        mentions
        |> ClientResult.map (fun mentions -> mentions.Tweets)
        |> ClientResult.map (fun tweets -> tweets |> Seq.filter (function
            | VideoRenderMention _ 
            | ImageRenderMention _ 
            | TextRenderMention _ 
            | GeneralRenderMention _ -> true
            | _ -> false ))
        
    //TODO: convert to SRTV tweet and send

    do! match mentions with
        | Success mentions ->
            match mentions.Meta.NextToken with
            | "" -> async { return () }
            | token -> handleMentions client startDate (Some token)
        | _ -> async { return () }

}

let isDevelopmentEnvironment () =
    let environmentVariable = Environment.GetEnvironmentVariable("NETCORE_ENVIRONMENT")
    String.IsNullOrEmpty(environmentVariable) || environmentVariable.ToLower() = "development"

let sendTweet (client:Client) =
    let tweet = SRTV.SRTVResponse.TextTweet "If you are reading this, the test tweet was successful"
    match client.TweetAsync tweet |> Async.RunSynchronously with
    | Success _ -> printfn "Sending tweet was successful..."
    | TwitterError (message, exn) ->
        printfn "Twitter error message: %s" message
        printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
    | OtherError (message, exn) ->
        printfn "Non-Twitter error message: %s" message
        printfn "Error: %O, Stack trace: %s" exn exn.StackTrace



let getTweet (client:Client) =
    let id = "1428036269356068882"
    match Seq.singleton id |> client.GetTweets |> Async.RunSynchronously with
    | Success tweetQuery ->
        printfn "Querying tweets was successful"
        printfn "Tweet ID is %s" tweetQuery.Tweets.[0].ID
        printfn "Tweet alt text is %s" tweetQuery.Includes.Media.[0].AltText
    | TwitterError (message, exn) ->
        printfn "Twitter error message: %s" message
        printfn "Error: %O, Stack trace: %s" exn exn.StackTrace
    | OtherError (message, exn) ->
        printfn "Non-Twitter error message: %s" message
        printfn "Error: %O, Stack trace %s" exn exn.StackTrace
    
[<EntryPoint>]
let main argv =

    let builder = ConfigurationBuilder()
    if isDevelopmentEnvironment()
    then
        match Assembly.Load(AssemblyName(AppDomain.CurrentDomain.FriendlyName)) with
        | null -> ()
        | assembly ->
            let config = builder.AddUserSecrets(assembly, true, true).Build()
            let client = Client(config)
            //sendTweet client
            getTweet client

    printfn "End of program..."
    0

    

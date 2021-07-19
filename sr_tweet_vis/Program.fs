// Learn more about F# at http://fsharp.org

open System

open SRTV.TweetMedia
open SRTV.TweetAudio
open SRTV.TweetImage

open System.IO

open System.Threading


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

type Listener(source:CancellationTokenSource, ?counter:int, ?startTime:DateTime) =
    
    let counter = Option.defaultValue 0 counter
    let startTime = 
        Option.defaultValue ( DateTime(2021, 7, 19).ToUniversalTime() ) startTime

    member __.Token = source.Token
    member __.Counter = counter
    member __.Date = startTime.ToLongTimeString()
    member __.Increment () = Listener (source, counter + 1, DateTime.UtcNow)
    member __.Cancel () =
        match counter with
        | 10 -> source.Cancel ()
        | _ -> ()

  
let rec loop (listener:Listener) = async {
    let interval = 1500
    do! Async.Sleep(interval)
    printfn $"Counter = {listener.Counter}, Date = {listener.Date}"
    listener.Cancel()
    return! loop (listener.Increment ())
}

let listen source = async {
    try
        let listener = Listener(source)
        do! loop listener
    finally
        printfn "Program is finished..."
}
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| imagefile; outputfile |]   -> printfn "Two args"; synthesize imagefile outputfile |> Async.RunSynchronously
    | [| outfile |]                 -> printfn "One arg"; toImage' outfile |> Async.RunSynchronously
    | _                             -> 
        printfn "No args"
        use cancellation = new CancellationTokenSource() 
        Async.Start( listen cancellation, cancellation.Token)
        Thread.Sleep(60 * 1000)
    0
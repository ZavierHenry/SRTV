namespace SRTV

open TweetMedia
open System.IO
open System

module TweetCaptions = 

    type Cue = { 
        startTime: TimeSpan
        endTime: TimeSpan
        text: string list
    }

    type SpokenWord = {
        start: TimeSpan
        text: string
    }

    let makeCue startTime endTime text : Cue =
        { startTime = startTime; endTime = endTime; text = text}

    let cueToString (cue:Cue) = 
        let words = List.rev cue.text |> String.concat " " 
        let format = $"hh\:mm\:ss\.fff"
        let start = cue.startTime.ToString(format)
        let stop = cue.endTime.ToString(format)

        $"%s{start} --> %s{stop}\n%s{words}"

    let maxSeconds = 3.0

    type Captions() =
        let mutable cues : Cue list = []
        let mutable words : SpokenWord list = []

        let oneMillisecondLater (timespan:TimeSpan) = 
            TimeSpan(timespan.Ticks + TimeSpan.TicksPerMillisecond * 1L)

        member this.add position text = 
            words <- { start = position; text = text } :: words

        member this.finalize (endPosition:TimeSpan) =
            let folder { start = start; text = text } = 
                function
                | []        -> 
                    makeCue start (oneMillisecondLater start) [text] 
                    |> List.singleton
                | { startTime = s } as cue :: rest when (start-s).TotalSeconds > maxSeconds ->
                    makeCue start (oneMillisecondLater start) [text]
                    :: { cue with endTime = start }
                    :: rest
                | { startTime = s; text = t} :: rest ->
                    makeCue s start (text::t) :: rest

            cues <- 
                match List.foldBack folder words [] with 
                | [] -> [] 
                | hd :: tl -> { hd with endTime = endPosition } :: tl

        member this.ToWebVTT() = 
            let printedCues = 
                List.rev cues
                |> List.map cueToString
                |> String.concat "\n\n"

            $"WEBVTT\n\n{printedCues}\n"

module TweetAudio =
    open Xabe.FFmpeg
    open System.Speech.Synthesis
    open System.Speech.AudioFormat
    open TweetCaptions
    open Utilities
    
    type Synthesizer() =

        let synth = new SpeechSynthesizer()
        let captions = Captions()

        let saveCaptions (e:SpeakProgressEventArgs) =
            captions.add e.AudioPosition e.Text

        let finalizeCaptions (e:BookmarkReachedEventArgs) =
            if e.Bookmark = "end of speech"
            then captions.finalize e.AudioPosition

        do 
            synth.Rate <- -1
            synth.SetOutputToNull()
            synth.SpeakProgress.Add(saveCaptions)
            synth.BookmarkReached.Add(finalizeCaptions)
            Path.GetDirectoryName(@"C:\FFMPEG\bin\ffmpeg.exe") |> FFmpeg.SetExecutablesPath

        member this.speak(words:string) = synth.Speak(words)
        member this.speak(builder:PromptBuilder) = synth.Speak(builder)

        member private this.speakToFile(mockTweet: MockTweet, filename: string) =
            let format = SpeechAudioFormatInfo(EncodingFormat.Pcm, 16000, 16, 1, 32000, 2, null)
            synth.SetOutputToWaveFile(filename, format)

            let builder = PromptBuilder()
            mockTweet.ToSpeakText() |> builder.AppendText
            builder.AppendBookmark("end of speech")
            this.speak(builder)

            synth.SetOutputToNull()

        member private this.captionsToFile(filename: string) =

            synth.SetOutputToNull()
            let vttfile = captions.ToWebVTT()
            printfn "%s" vttfile

            File.WriteAllText(filename, vttfile)

        member this.Synthesize(mockTweet: MockTweet, imagefile: string, outfile: string) = async {
            
            use tempAudioFile = new TempFile()
            use tempCaptionsFile = new TempFile()

            this.speakToFile(mockTweet, tempAudioFile.Path)
            this.captionsToFile(tempCaptionsFile.Path)

            let! videoInfo = FFmpeg.GetMediaInfo(imagefile) |> Async.AwaitTask
            let! audioInfo = FFmpeg.GetMediaInfo(tempAudioFile.Path) |> Async.AwaitTask

            let videoStream = Seq.head videoInfo.VideoStreams
            let videoStream = 
                videoStream.SetCodec(VideoCodec.libx264)
                    .AddSubtitles(tempCaptionsFile.Path, "utf-8", "BorderStyle=3")
                    :> IStream

            let audioStream = Seq.head audioInfo.AudioStreams
            let audioStream = audioStream.SetCodec(AudioCodec.aac) :> IStream

            let conversion = 
                FFmpeg.Conversions.New()
                    .UseMultiThread(false)
                    .AddParameter("-loop 1", ParameterPosition.PreInput)
                    .AddStream(videoStream, audioStream)
                    .AddParameter("-tune stillimage")
                    .UseShortest(true)
                    .SetPixelFormat(PixelFormat.yuv420p)
                    .SetAudioBitrate(192000L)
                    .SetOutput(outfile)
                    .SetOverwriteOutput(true)

            let! _ = conversion.Start() |> Async.AwaitTask
            return ()
        }

        interface IDisposable with
            member __.Dispose() = synth.Dispose()

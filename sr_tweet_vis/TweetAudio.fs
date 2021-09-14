namespace SRTV

open TweetMedia
open System.IO
open System
open System.Text.RegularExpressions

module TweetCaptions = 

    
    //type Cue = { 
    //    startTime: TimeSpan
    //    endTime: TimeSpan
    //    text: string list
    //}

    //type SpokenWord = {
    //    start: TimeSpan
    //    text: string
    //}

    //let makeCue startTime endTime text : Cue =
    //    { startTime = startTime; endTime = endTime; text = text}

    //let cueToString (cue:Cue) = 
    //    let words = List.rev cue.text |> String.concat " " 
    //    let format = $"hh\:mm\:ss\.fff"
    //    let start = cue.startTime.ToString(format)
    //    let stop = cue.endTime.ToString(format)

    //    $"%s{start} --> %s{stop}\n%s{words}"

    //let maxSeconds = 3.0

    //type Captions() =
    //    let mutable cues : Cue list = []
    //    let mutable words : SpokenWord list = []

    //    let oneMillisecondLater (timespan:TimeSpan) = 
    //        TimeSpan(timespan.Ticks + TimeSpan.TicksPerMillisecond * 1L)

    //    member this.add position text = 
    //        words <- { start = position; text = text } :: words

    //    member this.finalize (endPosition:TimeSpan) =
    //        let folder { start = start; text = text } = 
    //            function
    //            | []        -> 
    //                makeCue start (oneMillisecondLater start) [text] 
    //                |> List.singleton
    //            | { startTime = s } as cue :: rest when (start-s).TotalSeconds > maxSeconds ->
    //                makeCue start (oneMillisecondLater start) [text]
    //                :: { cue with endTime = start }
    //                :: rest
    //            | { startTime = s; text = t} :: rest ->
    //                makeCue s start (text::t) :: rest

    //        cues <- 
    //            match List.foldBack folder words [] with 
    //            | [] -> [] 
    //            | hd :: tl -> { hd with endTime = endPosition } :: tl

    //    member this.ToWebVTT() = 
    //        let printedCues = 
    //            List.rev cues
    //            |> List.map cueToString
    //            |> String.concat "\n\n"

    //        $"WEBVTT\n\n{printedCues}\n"

    type Captions() =
        let punctuation = @"(?<=[?.!])\s+"
        let [<Literal>] ChunkSize = 15

        static member toTimestamp seconds = TimeSpan.FromSeconds(seconds).ToString("hh\:mm\:ss\,fff")

        member __.HasLineOverflow text =
            Regex.Split(text, punctuation)
            |> Array.exists (fun line -> Regex.Matches(@"\s+", line).Count >= ChunkSize)

        member __.ToSilenceDetectionText text =
            let lines = Regex.Split(text, punctuation) |> Array.toList
            let captionLines =
                lines
                |> List.indexed
                |> List.collect (fun (index, line) ->
                    Regex.Split(line, @"\s+")
                    |> Array.chunkBySize ChunkSize
                    |> Array.map (String.concat " ")
                    |> Array.toList
                    |> List.map (fun x -> (index, x)))

            captionLines
            |> List.map (fun (lineIndex, text) ->
                match List.filter (fun (i, _) -> i = lineIndex) captionLines with
                | [] | [ _ ]  -> text
                | _         -> Regex.Replace(text, @"(?<![.!?])$", "."))
            |> String.concat " "

        member __.ToCaptions(text, timestamps) =
            let lines = Regex.Split(text, punctuation) |> Array.toList
            let captionLines =
                lines
                |> List.indexed
                |> List.collect (fun (index, line) ->
                    Regex.Split(line, @"\s+")
                    |> Array.chunkBySize ChunkSize
                    |> Array.map (String.concat " ")
                    |> Array.toList
                    |> List.map (fun x -> (index, x)))
            let captionLines =
                captionLines
                |> List.map (fun (lineIndex, text) ->
                    match List.filter (fun (i, _) -> i = lineIndex) captionLines with
                    | [] | [ _ ]  -> (lineIndex, text)
                    | _         -> (lineIndex, Regex.Replace(text, @"(?<![.!?])$", ".")))

            timestamps
            |> List.zip captionLines.[ 0 .. timestamps.Length - 1 ]
            |> List.indexed
            |> List.map (fun (index, ((lineIndex, text), ts)) ->
                let endTime = if lines.[lineIndex] = text then ts else ts - 0.5
                let startTime =
                    match index with
                    | 0 -> 0.0
                    | i when lines.[fst captionLines.[i-1]] <> snd captionLines.[i-1] ->
                        timestamps.[i-1] - 0.5
                    | i -> timestamps.[i-1]
                let text =
                    if lines.[lineIndex] <> text then Regex.Replace(text, @".$", "") else text
                $"{index+1}\n%s{Captions.toTimestamp startTime} --> %s{Captions.toTimestamp <| endTime - 0.001}\n{text}")
            |> String.concat "\n\n"

module TweetAudio =
    open Xabe.FFmpeg
    open System.Speech.Synthesis
    open System.Speech.AudioFormat

    open TweetCaptions
    open Utilities

    open System.Diagnostics

    type TTS() =
        let location = @"C:\TTS\Scripts\tts.exe"
        let modelName = "tts_models/en/ljspeech/vits"

        member private __.buildCoquiProcess text outpath = 
            let startInfo =
                ProcessStartInfo(
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    FileName = location,
                    RedirectStandardOutput = true
                )

            startInfo.ArgumentList.Add("--text")
            startInfo.ArgumentList.Add(text)

            startInfo.ArgumentList.Add("--model_name")
            startInfo.ArgumentList.Add(modelName)

            startInfo.ArgumentList.Add("--out_path")
            startInfo.ArgumentList.Add(outpath)

            new Process(StartInfo = startInfo)

        member this.Speak(text: string, outpath: string) = async {
            use proc = this.buildCoquiProcess text outpath
            proc.Start() |> ignore
            do! proc.WaitForExitAsync() |> Async.AwaitTask
        }

    type FFMPEG() =
        
        let threshold = -40
        let duration = 0.5
        let bitrate = 192000L

        do
            Path.GetDirectoryName(@"C:\FFMPEG\bin\ffmpeg.exe") |> FFmpeg.SetExecutablesPath

        member __.SilenceDetect(audioFilename: string) = async {
            let mutable timestamps : float list = []

            let! audioInfo = FFmpeg.GetMediaInfo(audioFilename) |> Async.AwaitTask
            let audioStream = Seq.head audioInfo.AudioStreams
            
            let conversion =
                FFmpeg.Conversions.New()
                    .UseMultiThread(false)
                    .AddStream(audioStream)
                    .AddParameter($"-af silencedetect=n={threshold}dB:d={duration}")
                    .SetOutputFormat("null")
                    .AddParameter("-")

            conversion.OnDataReceived.AddHandler(
                fun _ e ->
                    let m = Regex.Match(e.Data, @"silence_end:\s+(?<time>\d+\.\d+)")
                    if m.Success then timestamps <- float m.Groups.["time"].Value :: timestamps)

            do! conversion.Start() |> Async.AwaitTask |> Async.Ignore
            return timestamps
        }

        member __.MakeVideo(audioFile :string, subtitleFile: string, imageFile: string, outFile:string) = async {
            
            let! audioInfo = FFmpeg.GetMediaInfo(audioFile) |> Async.AwaitTask
            let audioStream = 
                (Seq.head audioInfo.AudioStreams)
                    .SetCodec(AudioCodec.aac)
                    :> IStream
            
            let! videoInfo = FFmpeg.GetMediaInfo(imageFile) |> Async.AwaitTask
            let videoStream =
                (Seq.head videoInfo.VideoStreams)
                    .SetCodec(VideoCodec.libx264)
                    .AddSubtitles(subtitleFile, "utf-8", "BorderStyle=3")
                    :> IStream

            let conversion =
                FFmpeg.Conversions.New()
                    .UseMultiThread(false)
                    .AddParameter("-loop 1", ParameterPosition.PreInput)
                    .AddStream(videoStream, audioStream)
                    .AddParameter("-tune stillimage")
                    .UseShortest(true)
                    .SetPixelFormat(PixelFormat.yuv420p)
                    .SetAudioBitrate(bitrate)
                    .SetOutput(outFile)
                    .SetOverwriteOutput(true)

            do! conversion.Start() |> Async.AwaitTask |> Async.Ignore
        }

    type Synthesizer() =

        //let synth = new SpeechSynthesizer()

        let captions = Captions()
        let engine = TTS()
        let ffmpeg = FFMPEG()

        //let saveCaptions (e:SpeakProgressEventArgs) =
        //    captions.add e.AudioPosition e.Text

        //let finalizeCaptions (e:BookmarkReachedEventArgs) =
        //    if e.Bookmark = "end of speech"
        //    then captions.finalize e.AudioPosition

        //do
        //    Path.GetDirectoryName(@"C:\FFMPEG\bin\ffmpeg.exe") |> FFmpeg.SetExecutablesPath

        //do 
        //    synth.Rate <- -1
        //    synth.SetOutputToNull()
        //    synth.SpeakProgress.Add(saveCaptions)
        //    synth.BookmarkReached.Add(finalizeCaptions)
        //    Path.GetDirectoryName(@"C:\FFMPEG\bin\ffmpeg.exe") |> FFmpeg.SetExecutablesPath
            

        //member this.speak(words:string) = synth.Speak(words)

        //member this.speak(builder:PromptBuilder) = synth.Speak(builder)

        member this.Speak(words: string, filename: string) = async {
            do! engine.Speak(words, filename)
        }

        //member private this.speakToFile(mockTweet: MockTweet, filename: string) =
        //    this.Speak(mockTweet, filename)


        //member private this.speakToFile(mockTweet: MockTweet, filename: string) =
        //    let format = SpeechAudioFormatInfo(EncodingFormat.Pcm, 16000, 16, 1, 32000, 2, null)
        //    synth.SetOutputToWaveFile(filename, format)

        //    let builder = PromptBuilder()
        //    mockTweet.ToSpeakText() |> builder.AppendText
        //    builder.AppendBookmark("end of speech")
        //    this.speak(builder)

        //    synth.SetOutputToNull()

        //member private this.captionsToFile(filename: string) =

        //    //synth.SetOutputToNull()
        //    //let vttfile = captions.ToWebVTT()
        //    let subtitles = captions.ToString()
        //    File.WriteAllText(filename, subtitles)

        //    //File.WriteAllText(filename, vttfile)

        //member private this.captionsToFile(filename: string) = async {
        //    let subtitles = captions.ToString()
        //    do! File.WriteAllTextAsync(filename, subtitles) |> Async.AwaitTask
        //}

        member this.Synthesize(speakText: string, imagefile: string, outfile: string) = async {
            
            use tempAudioFile = new TempFile()

            do! this.Speak(speakText, tempAudioFile.Path)
            let! timestamps =
                match captions.HasLineOverflow(speakText) with
                | true ->
                    use tempCaptionsAudioFile = new TempFile()
                    let captionText = captions.ToSilenceDetectionText(speakText)
                    async {
                        do! this.Speak(captionText, tempCaptionsAudioFile.Path)
                        return! ffmpeg.SilenceDetect(tempCaptionsAudioFile.Path)
                    }
                | false -> ffmpeg.SilenceDetect(tempAudioFile.Path)

            let subtitles = captions.ToCaptions(speakText, timestamps)
            use captionsFile = new TempFile()
            do! File.WriteAllTextAsync(captionsFile.Path, subtitles) |> Async.AwaitTask

            do! ffmpeg.MakeVideo(tempAudioFile.Path, captionsFile.Path, imagefile, outfile)

            //use tempAudioFile = new TempFile()
            //use tempCaptionsFile = new TempFile()

            ////this.speakToFile(mockTweet, tempAudioFile.Path)
            //do! this.captionsToFile(tempCaptionsFile.Path)

            //let! videoInfo = FFmpeg.GetMediaInfo(imagefile) |> Async.AwaitTask
            //let! audioInfo = FFmpeg.GetMediaInfo(tempAudioFile.Path) |> Async.AwaitTask

            //let videoStream = Seq.head videoInfo.VideoStreams
            //let videoStream = 
            //    videoStream.SetCodec(VideoCodec.libx264)
            //        .AddSubtitles(tempCaptionsFile.Path, "utf-8", "BorderStyle=3")
            //        :> IStream

            //let audioStream = Seq.head audioInfo.AudioStreams
            //let audioStream = audioStream.SetCodec(AudioCodec.aac) :> IStream

            //let conversion = 
            //    FFmpeg.Conversions.New()
            //        .UseMultiThread(false)
            //        .AddParameter("-loop 1", ParameterPosition.PreInput)
            //        .AddStream(videoStream, audioStream)
            //        .AddParameter("-tune stillimage")
            //        .UseShortest(true)
            //        .SetPixelFormat(PixelFormat.yuv420p)
            //        .SetAudioBitrate(192000L)
            //        .SetOutput(outfile)
            //        .SetOverwriteOutput(true)

            //let! _ = conversion.Start() |> Async.AwaitTask
            //return ()
        }

        //interface IDisposable with
        //    member __.Dispose() = ()

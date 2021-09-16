namespace SRTV

open TweetMedia
open System.IO
open System
open System.Text.RegularExpressions

module TweetCaptions =

    type Captions() =
        let punctuation = @"(?<=[?.!])\s+"
        let [<Literal>] ChunkSize = 15
        let [<Literal>] SilenceDelay = 0.55

        static member toTimestamp seconds = TimeSpan.FromSeconds(seconds).ToString("hh\:mm\:ss\,fff")

        member __.HasLineOverflow text =
            Regex.Split(text, punctuation)
            |> Array.exists (fun line -> Regex.Matches(line, @"\s+").Count >= ChunkSize)

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
            |> List.zip captionLines.[ 0 .. timestamps.Length - 1]
            |> List.indexed
            |> List.map (fun (index, ((lineIndex, text), ts)) ->
                let endTime = 
                    captionLines.[0 .. index] 
                    |> List.filter (fun (lineIndex, text) -> lines.[lineIndex] <> text)
                    |> List.length
                    |> fun x -> ts - (SilenceDelay * float x)

                let startTime =
                    match index with
                    | 0 -> 0.0
                    | i ->
                        captionLines.[0 .. i - 1]
                        |> List.filter (fun (lineIndex, text) -> lines.[lineIndex] <> text)
                        |> List.length
                        |> fun x -> timestamps.[i-1] - (SilenceDelay * float x)
                let text =
                    if lines.[lineIndex] <> text then Regex.Replace(text, @".$", "") else text
                $"{index+1}\n%s{Captions.toTimestamp startTime} --> %s{Captions.toTimestamp <| endTime - 0.001}\n{text}")
            |> String.concat "\n\n"

module TweetAudio =
    open Xabe.FFmpeg

    open TweetCaptions
    open Utilities

    open System.Diagnostics

    type TTS() =
        let modelName = "tts_models/en/ljspeech/vits"
        let [<Literal>] EnvironmentVariable = "TTS_EXECUTABLE"

        let filename = 
            tryFindEnvironmentVariable EnvironmentVariable 
            |> Option.orElse (tryFindExecutableNameOnPath "tts") 
            |> Option.orElseWith(fun () -> failwithf "Cannot find TTS engine in either %s env variable or in PATH" EnvironmentVariable)
            |> Option.get

        member private __.buildCoquiProcess text outpath = 
            let startInfo =
                ProcessStartInfo(
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    FileName = filename,
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

        let [<Literal>] EnvironmentVariable = "FFMPEG_EXECUTABLE"

        do
            tryFindEnvironmentVariable(EnvironmentVariable)
            |> Option.orElse (tryFindExecutableNameOnPath "ffmpeg")
            |> Option.iter (Path.GetDirectoryName >> FFmpeg.SetExecutablesPath)

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
            return timestamps |> List.rev
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

        let captions = Captions()
        let engine = TTS()
        let ffmpeg = FFMPEG()

        member __.Speak(words: string, filename: string) = async {
            do! engine.Speak(words, filename)
        }

        member this.Synthesize(speakText: string, outfile: string) = async {
            
            use tempAudioFile = new TempFile()

            do! this.Speak(speakText, tempAudioFile.Path)
            let! timestamps =
                match captions.HasLineOverflow(speakText) with
                | true ->
                    async {
                        use tempCaptionsAudioFile = new TempFile()
                        let captionText = captions.ToSilenceDetectionText(speakText)
                        do! this.Speak(captionText, tempCaptionsAudioFile.Path)
                        return! ffmpeg.SilenceDetect(tempCaptionsAudioFile.Path)
                    }
                | false -> ffmpeg.SilenceDetect(tempAudioFile.Path)

            let subtitles = captions.ToCaptions(speakText, timestamps)
            use captionsFile = new TempFile()
            do! File.WriteAllTextAsync(captionsFile.Path, subtitles) |> Async.AwaitTask

            let imageFile = Path.Join(Environment.CurrentDirectory, "assets", "black_rect.jpg")
            do! ffmpeg.MakeVideo(tempAudioFile.Path, captionsFile.Path, imageFile, outfile)
        }
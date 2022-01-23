namespace SRTV

open TweetMedia
open System.IO
open System
open System.Text.RegularExpressions

module TweetAudio =

    open Xabe.FFmpeg
    open System.Diagnostics

    open Utilities

    type Silence = 
        | Silence of float * float
        
        static member duration = function | Silence (_, dur) -> dur
        static member endSeconds = function | Silence (es, _) -> es

    type Captions() =
        let punctuation = @"(?<=[?.!])\s+"
        let [<Literal>] ChunkSize = 15

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

            let captionDelay = 
                captionLines.[ 0 .. List.length timestamps - 1]
                |> List.indexed
                |> List.scan (fun delay (index, (lineIndex, text)) -> delay + if lines.[lineIndex] <> text then Silence.duration timestamps.[index] else 0.0) 0.0
                |> List.tail


            timestamps
            |> List.zip captionLines.[ 0 .. timestamps.Length - 1]
            |> List.indexed
            |> List.map (fun (index, ((lineIndex, text), Silence (ts, _))) ->
                let endTime = ts - captionDelay.[index]

                let startTime =
                    match index with
                    | 0 -> 0.0
                    | i -> Silence.endSeconds timestamps.[i-1] - captionDelay.[i-1]

                let text =
                    if lines.[lineIndex] <> text then Regex.Replace(text, @".$", "") else text
                $"{index+1}\n%s{Captions.toTimestamp startTime} --> %s{Captions.toTimestamp <| endTime - 0.001}\n{text}")
            |> String.concat "\n\n"

    type TTS() =
        let [<Literal>] TTSFileEnvironmentVariable = "TTS_PATH"

        member private __.buildTTSProcess text outpath = 
            let ttsFilename = 
                tryFindEnvironmentVariable TTSFileEnvironmentVariable
                |> Option.orElseWith (fun () -> failwithf "TTS executable environment variable %s not set" TTSFileEnvironmentVariable)
                |> Option.get

            let startInfo =
                ProcessStartInfo(
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    FileName = "python",
                    RedirectStandardError = true
                )

            startInfo.ArgumentList.Add(ttsFilename)
            startInfo.ArgumentList.Add(text)
            startInfo.ArgumentList.Add(outpath)

            new Process(StartInfo = startInfo)

        member this.Speak(text: string, outpath: string) = async {
            use proc = this.buildTTSProcess text outpath
            if not <| proc.Start() then printf "TTS process did not start"
            proc.WaitForExit()
            if proc.ExitCode <> 0 then proc.StandardError.ReadToEnd() |> printf "Error in process: %s"
        }

    type FFMPEG() =
        
        let threshold = -40
        let duration = 0.3
        let bitrate = 192000L

        let [<Literal>] EnvironmentVariable = "FFMPEG_EXECUTABLE"

        let silenceEndRegex = Regex(@"silence_end:\s+(?<time>\d+\.\d+)")
        let durationRegex = Regex(@"silence_duration:\s+(?<duration>\d+\.\d+)")

        do
            tryFindEnvironmentVariable(EnvironmentVariable)
            |> Option.orElse (tryFindExecutableNameOnPath "ffmpeg")
            |> Option.iter (Path.GetDirectoryName >> FFmpeg.SetExecutablesPath)

        member __.SilenceDetect(audioFilename: string) = async {
            let mutable timestamps : Silence list = []

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
                    let silenceEnd = silenceEndRegex.Match(e.Data)
                    let duration = durationRegex.Match(e.Data)

                    if silenceEnd.Success && duration.Success 
                    then
                        let se = silenceEnd.Groups.["time"].Value
                        let duration = duration.Groups.["duration"].Value
                        timestamps <- Silence (float se, float duration) :: timestamps)

            do! conversion.Start() |> Async.AwaitTask |> Async.Ignore
            return timestamps |> List.sortBy (fun (Silence (se, _)) -> se)
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
                    .SetOutputFormat(Format.mp4)
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
            File.WriteAllText(captionsFile.Path, subtitles)

            let imageFile = Path.Join(Environment.CurrentDirectory, "assets", "black_rect.jpg")
            do! ffmpeg.MakeVideo(tempAudioFile.Path, captionsFile.Path, imageFile, outfile)
        }

        member this.Synthesize(mockTweet: MockTweet, outfile: string, ref: DateTime, renderOptions: RenderOptions) =
            let text = 
                match renderOptions with
                | Video Version.Full | Text Version.Full | Image (_, Version.Full) ->
                    mockTweet.ToFullSpeakText(ref)
                | Video Version.Regular | Text Version.Regular | Image (_, Version.Regular) ->
                    mockTweet.ToSpeakText(ref)

            this.Synthesize(text, outfile)
            
            
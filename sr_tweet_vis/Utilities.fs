namespace SRTV

module Utilities =
    open System
    open System.IO

    type Theme = 
        Light | Dim | Dark
        static member toAttributeValue = function
            | Light -> "light"
            | Dim -> "dim"
            | Dark -> "dark"

        static member fromAttributeValue = function
            | "light"   -> Some Theme.Light
            | "dim"     -> Some Theme.Dim
            | "dark"    -> Some Theme.Dark
            | _         -> None

    type Version = | Regular | Full

    type RenderOptions =
        | Video of version:Version
        | Image of theme:Theme * version:Version
        | Text of version:Version
    
    type TempFile() =
        let path = Path.GetTempFileName()
        member this.Path = path

        interface IDisposable with
            member this.Dispose() = File.Delete(path)

    let nullableSequenceToValue<'a> = function
        | null -> Seq.empty<'a>
        | sequence -> sequence

    let tryNonBlankString str = Some str |> Option.filter (not << String.IsNullOrEmpty)

    let tryFindEnvironmentVariable var =
        Environment.GetEnvironmentVariable(var) 
        |> Some
        |> Option.filter (function | null | "" -> false | _ -> true)

    let tryFindExecutableNameOnPath name =
        match Environment.GetEnvironmentVariable("PATH") with | null -> "" | paths -> paths
        |> fun path -> path.Split(Path.PathSeparator)
        |> Seq.tryFind (fun dir -> Path.Join(dir, name) |> File.Exists || Path.Join(dir, $"{name}.exe") |> File.Exists)

    module DateTimePatterns =

        let private toTimeAgo (span:float) =
            Some span |> Option.filter (fun span -> span >= 0.0) |> Option.map int

        let private toTimeFromNow (span:float) =
            toTimeAgo (-span)

        let (|BeforeThisYear|_|) (now:DateTime) (datetime:DateTime) =
            let x = now - datetime
            Some (datetime.Year, datetime.Month, datetime.Day)
            |> Option.filter (fun (y, _, _) -> now.Year > y)
    
        let (|BeforeThisWeek|_|) (now:DateTime) (datetime:DateTime) =
            Some (datetime.Year, datetime.Month, datetime.Day) 
            |> Option.filter (fun _ -> (now-datetime).TotalDays > 7.0)

        let (|DaysAgo|_|) (now:DateTime) (datetime:DateTime) =
            toTimeAgo (now-datetime).TotalDays

        let (|HoursAgo|_|) (now:DateTime) (datetime:DateTime) =
            toTimeAgo (now-datetime).TotalHours

        let (|MinutesAgo|_|) (now:DateTime) (datetime:DateTime) =
            toTimeAgo (now-datetime).TotalMinutes
            
        let (|SecondsAgo|_|) (now:DateTime) (datetime:DateTime) =
            toTimeAgo (now-datetime).TotalSeconds

        let (|SecondsFromNow|_|) (now:DateTime) (datetime:DateTime) =
            toTimeFromNow (now-datetime).TotalSeconds

        let (|MinutesFromNow|_|) (now:DateTime) (datetime:DateTime) =
            toTimeFromNow (now-datetime).TotalMinutes

        let (|HoursFromNow|_|) (now:DateTime) (datetime:DateTime) =
            toTimeFromNow (now-datetime).TotalHours

        let (|DaysFromNow|_|) (now:DateTime) (datetime:DateTime) =
            toTimeFromNow (now-datetime).TotalDays









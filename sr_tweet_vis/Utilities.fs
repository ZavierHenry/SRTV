namespace SRTV

module Utilities =
    open System
    open System.IO

    type TempFile() =
        let path = Path.GetTempFileName()
        member this.Path = path

        interface IDisposable with
            member this.Dispose() = File.Delete(path)

    let pluralize (singular:string) plural = function | 1 -> singular | _ -> plural
    let pluralizePrefix prefix = pluralize prefix $"{prefix}s"
    let tryNonBlankString str = Some str |> Option.filter (not << String.IsNullOrEmpty)

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









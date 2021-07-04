namespace SRTV

module Utilities =
    open System
    open System.IO

    type TempFile() =
        let path = Path.GetTempFileName()
        member this.Path = path

        interface IDisposable with
            member this.Dispose() = File.Delete(path)


    let pluralize singular plural number : string = 
        if number = 1 then singular else plural


    module DateTimeUtil = 

        let (|BeforeThisYear|_|) (now:DateTime) (datetime:DateTime) =
            if now.Year < datetime.Year 
            then Some(datetime.Year, datetime.Month, datetime.Day) 
            else None
    
        let (|BeforeThisWeek|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = now - datetime
            if now.Year = datetime.Year && timespan.TotalDays >= 7.0 
            then Some(datetime.Month, datetime.Day) 
            else None

        let (|BeforeToday|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = now - datetime
            if timespan.Days > 0 && timespan.Days < 7
            then Some(timespan.Days)
            else None

        let (|BeforeThisHour|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = now - datetime
            if timespan.Hours > 0 && timespan.TotalHours < 24.0
            then Some(timespan.Hours)
            else None

        let (|BeforeThisMinute|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = now - datetime
            if timespan.Minutes > 0 && timespan.TotalMinutes < 60.0
            then Some(timespan.Minutes)
            else None

        let (|BeforeThisSecond|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = now - datetime
            if timespan.Seconds > 0 && timespan.TotalSeconds < 60.0
            then Some(timespan.Seconds)
            else None

        let (|InLessThanOneMinute|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = datetime - now
            if timespan.Seconds > 0 && timespan.Minutes = 0
            then Some(timespan.Seconds)
            else None

        let (|InLessThanOneHour|_|) (now:DateTime) (datetime:DateTime) =
            let timespan = datetime - now
            if timespan.Minutes > 0 && timespan.Hours = 0 
            then Some(timespan.Minutes) 
            else None









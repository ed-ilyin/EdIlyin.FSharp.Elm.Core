namespace EdIlyin.FSharp.Elm.Core

open System


module Debug =
    let log label x =
        do printfn "%s: %A" label x
        x

    let logToFile filename label x =
        let now = DateTime.Now
        let today = now.ToString(@"yyyyMMdd")
        let filename = sprintf "%s%s.log" filename today
        let timestamp = now.ToString(@"HH:mm:ss.fffffff")

        do sprintf "%s: %s:\n%A\n\n" timestamp label x
            |> curry IO.File.AppendAllText filename

        x

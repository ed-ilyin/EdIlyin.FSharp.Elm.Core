namespace EdIlyin.Elm.Core

open System
open MailKit.Net.Smtp
open MailKit
open MimeKit
open Hopac
open Hopac.Infixes


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

    let mail origin subject x =
        let client = new SmtpClient ()
        let connect = client.ConnectAsync "mx01.azurepack.com"

        let task =
            connect.ContinueWith
                (fun _ ->
                    try
                        let bodyText = sprintf "%A" x
                        let message = MimeMessage ()

                        do sprintf "%s@provengine.azure" origin
                            |> MailboxAddress
                            |> message.From.Add

                        do MailboxAddress "integration@nervogrid.com"
                            |> message.To.Add

                        do message.Subject <-
                            sprintf "%s: %s" origin subject

                        let body = TextPart "plain"
                        do body.Text <- bodyText
                        do message.Body <- body
                        let send = client.Send message
                        do client.Disconnect true
                        do client.Dispose ()

                    with | exn ->
                        logToFile origin
                            (sprintf "Error sending mail: %s:\n%A"
                                subject
                                x
                            )
                            exn.Message
                            |> ignore
                )

        x

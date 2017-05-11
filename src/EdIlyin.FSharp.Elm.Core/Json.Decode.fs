namespace EdIlyin.FSharp.Elm.Core.Json

open Chiron
open EdIlyin.FSharp.Elm.Core


module Decode =
    let string =
        let label = "string"

        let decFn json =
            let err = sprintf "Unexpected '%A'" json

            match json with
                | String s ->
                    try string s |> Ok
                    with | _ -> label => err |> Err

                | _ -> label => err |> Err

        {label = label; decoder = decFn}

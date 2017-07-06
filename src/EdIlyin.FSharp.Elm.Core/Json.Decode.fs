namespace EdIlyin.FSharp.Elm.Core.Json

open Chiron
open EdIlyin.FSharp.Elm.Core


module Decode =
    let string =
        let label = "a String"
        Decode.primitive label
            (fun value ->
                match value with
                    | String str ->
                        match str with
                            | null -> label => value |> Decode.ExpectingButGot
                            | s -> Decode.Decoded s

                    | _ -> label => value |> Decode.ExpectingButGot
            )


    let value : Decode.Decoder<Json, Json> =
        Decode.primitive "an JSON Value" Decode.Decoded

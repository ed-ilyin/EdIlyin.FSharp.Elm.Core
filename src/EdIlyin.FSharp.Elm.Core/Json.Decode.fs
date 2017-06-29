namespace EdIlyin.FSharp.Elm.Core.Json

open Chiron
open EdIlyin.FSharp.Elm.Core


module Decode =
    let string =
        let label = "a String"
        Decode.primitive label
            (fun value ->
                let unexpected = sprintf "%A" json
                
                match value with
                    | String s -> Ok s
                    | _ -> label => unexpected |> Err
            )


    let value : Decoder<Json, Json> =
        Decode.primitive "an JSON Value" Ok
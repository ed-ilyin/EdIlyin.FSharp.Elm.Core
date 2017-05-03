namespace EdIlyin.FSharp.Elm.Core


type DecoderLabel = string


type DecoderError = string


type Decoder<'a,'b> = {
    decoder : ('a -> Result<DecoderLabel * DecoderError, 'b>)
    label:  DecoderLabel
}


module Decode =
    let run decoder value =
        decoder.decoder value


    let getLabel decoder =
        decoder.label


    let decodeValue decoder value =
        match run decoder value with
            | Err (label, error) ->
                sprintf "Expecting %s %s" label error |> Err

            | Ok value' -> Ok value'


    let succeed x = {
        decoder = (fun _ -> Ok x)
        label = sprintf "%A" x
    }


    let failed x = {
        decoder = (fun _ -> Err x)
        label = sprintf "%A" x
    }


    let primitive func =
        let label = "primitive"

        let decoderFn input =
            match func input with
                | Err error -> Err error
                | Ok value -> Ok value

        { decoder = decoderFn; label = label }


    let setLabel label decoder = { decoder with label = label }


    let (<?>) decoder label = setLabel label decoder


    let andThen func decoder =
        primitive
            (fun input ->
                match run decoder input with
                    | Err (label, error) -> Err (label, error)
                    | Ok value -> run (func value) input
            )


    let (>>=) decoder func = andThen func decoder


    let andMap decoder functionDecoder =
        functionDecoder >>= (fun f -> decoder >>= (f >> succeed))


    let (<*>) fnDecoder decoder = andMap decoder fnDecoder


    let map func decoder =
        succeed func
            <*> decoder
            <?> sprintf "{ %s }" (getLabel decoder)


    let map2 func decoder1 decoder2 =
        succeed func
            <*> decoder1
            <*> decoder2
            <?> sprintf "{ %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)


    let map3 func decoder1 decoder2 decoder3 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <?> sprintf "{ %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)


    let map4 func decoder1 decoder2 decoder3 decoder4 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <?> sprintf "{ %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)


    let fromResult result =
        Result.unpack failed succeed result

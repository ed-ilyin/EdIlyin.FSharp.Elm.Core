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
        decoder = (fun _ -> "nothing" => x |> Err)
        label = sprintf "%A" x
    }


    let primitive label func =
        let decoderFn input =
            match func input with
                | Err error -> Err error
                | Ok value -> Ok value

        { decoder = decoderFn; label = label }


    let setLabel label decoder = { decoder with label = label }


    let (<?>) decoder label = setLabel label decoder


    let andThen func decoder =
        let label = getLabel decoder |> sprintf "and then %s"
        primitive label
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


    let map5 func decoder1 decoder2 decoder3 decoder4 decoder5 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <*> decoder5
            <?> sprintf "{ %s, %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)
                (getLabel decoder5)


    let map6 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <*> decoder5
            <*> decoder6
            <?> sprintf "{ %s, %s, %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)
                (getLabel decoder5)
                (getLabel decoder6)


    let map7 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <*> decoder5
            <*> decoder6
            <*> decoder7
            <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)
                (getLabel decoder5)
                (getLabel decoder6)
                (getLabel decoder7)


    let map8 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <*> decoder5
            <*> decoder6
            <*> decoder7
            <*> decoder8
            <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)
                (getLabel decoder5)
                (getLabel decoder6)
                (getLabel decoder7)
                (getLabel decoder8)


    let map9 func decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 =
        succeed func
            <*> decoder1
            <*> decoder2
            <*> decoder3
            <*> decoder4
            <*> decoder5
            <*> decoder6
            <*> decoder7
            <*> decoder8
            <*> decoder9
            <?> sprintf "{ %s, %s, %s, %s, %s, %s, %s, %s, %s }"
                (getLabel decoder1)
                (getLabel decoder2)
                (getLabel decoder3)
                (getLabel decoder4)
                (getLabel decoder5)
                (getLabel decoder6)
                (getLabel decoder7)
                (getLabel decoder8)
                (getLabel decoder9)


    let fromResult result =
        Result.unpack failed succeed result


    /// Match an input token if the predicate is satisfied
    let satisfy nextFn predicate label =
        let innerFn input =
            let lab =
                Option.Extra.unwrap

            let remainingInput, opt = nextFn input

            match opt with
                | None ->
                    let err = "No more input"
                    Err (label,err)

                | Some first ->
                    match predicate first with
                        | Ok value -> value => remainingInput |> Ok

                        | Err unexpected ->
                            label =>
                                sprintf "Unexpected '%A'." unexpected
                                    |> Err

        // return the parser
        {decoder=innerFn;label=label}


    /// Run the parser on a InputState
    let runOnInput parser input =
        // call inner function with input
        parser.decoder input


    let parseAny parser input =
         runOnInput parser input
            |> Result.mapError (uncurry (sprintf "Expecting %A. %A."))
            |> Result.map fst


    let tuple p1 p2 =
        let label = sprintf "%s and %s" (getLabel p1) (getLabel p2)
        p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            succeed (p1Result,p2Result) ))
        <?> label


    let (.>>.) = tuple


    let (>>.) p1 p2 =
        // create a pair
        p1 .>>. p2
        // then only keep the second value
        |> map snd

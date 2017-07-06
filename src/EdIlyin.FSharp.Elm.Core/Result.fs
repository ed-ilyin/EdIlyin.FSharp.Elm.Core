namespace EdIlyin.FSharp.Elm.Core


module Result =
    let andThen func result =
        match result with
            | Ok value -> func value
            | Error message -> Error message


    let bind = andThen


    let map func ra =
        match ra with
            | Ok a -> Ok (func a)
            | Error x -> Error x


    let map2 func ra rb =
        match (ra,rb) with
            | (Ok a, Ok b) -> Ok (func a b)
            | (Error x, _) -> Error x
            | (_, Error x) -> Error x


    let map3 func ra rb rc =
        match (ra,rb,rc) with
            | (Ok a, Ok b, Ok c) -> Ok (func a b c)
            | (Error x, _, _) -> Error x
            | (_, Error x, _) -> Error x
            | (_, _, Error x) -> Error x


    let map4 func ra rb rc rd =
        match (ra,rb,rc,rd) with
            | (Ok a, Ok b, Ok c, Ok d) -> Ok (func a b c d)
            | (Error x, _, _, _) -> Error x
            | (_, Error x, _, _) -> Error x
            | (_, _, Error x, _) -> Error x
            | (_, _, _, Error x) -> Error x


    let map5 func ra rb rc rd re =
        match (ra,rb,rc,rd,re) with
            | (Ok a, Ok b, Ok c, Ok d, Ok e) -> Ok (func a b c d e)
            | (Error x, _, _, _, _) -> Error x
            | (_, Error x, _, _, _) -> Error x
            | (_, _, Error x, _, _) -> Error x
            | (_, _, _, Error x, _) -> Error x
            | (_, _, _, _, Error x) -> Error x


    let map6 func ra rb rc rd re rf =
        match (ra,rb,rc,rd,re,rf) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f) -> Ok (func a b c d e f)
            | (Error x, _, _, _, _, _) -> Error x
            | (_, Error x, _, _, _, _) -> Error x
            | (_, _, Error x, _, _, _) -> Error x
            | (_, _, _, Error x, _, _) -> Error x
            | (_, _, _, _, Error x, _) -> Error x
            | (_, _, _, _, _, Error x) -> Error x


    let map8 func ra rb rc rd re rf rg rh =
        match (ra,rb,rc,rd,re,rf,rg,rh) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h) -> Ok (func a b c d e f g h)
            | (Error x, _, _, _, _, _, _, _) -> Error x
            | (_, Error x, _, _, _, _, _, _) -> Error x
            | (_, _, Error x, _, _, _, _, _) -> Error x
            | (_, _, _, Error x, _, _, _, _) -> Error x
            | (_, _, _, _, Error x, _, _, _) -> Error x
            | (_, _, _, _, _, Error x, _, _) -> Error x
            | (_, _, _, _, _, _, Error x, _) -> Error x
            | (_, _, _, _, _, _, _, Error x) -> Error x


    let map9 func ra rb rc rd re rf rg rh ri =
        match (ra,rb,rc,rd,re,rf,rg,rh,ri) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h,Ok i) -> Ok (func a b c d e f g h i)
            | (Error x, _, _, _, _, _, _, _, _) -> Error x
            | (_, Error x, _, _, _, _, _, _, _) -> Error x
            | (_, _, Error x, _, _, _, _, _, _) -> Error x
            | (_, _, _, Error x, _, _, _, _, _) -> Error x
            | (_, _, _, _, Error x, _, _, _, _) -> Error x
            | (_, _, _, _, _, Error x, _, _, _) -> Error x
            | (_, _, _, _, _, _, Error x, _, _) -> Error x
            | (_, _, _, _, _, _, _, Error x, _) -> Error x
            | (_, _, _, _, _, _, _, _, Error x) -> Error x


    let map10 func ra rb rc rd re rf rg rh ri rj =
        match (ra,rb,rc,rd,re,rf,rg,rh,ri,rj) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h,Ok i,Ok j) -> Ok (func a b c d e f g h i j)
            | (Error x, _, _, _, _, _, _, _, _, _) -> Error x
            | (_, Error x, _, _, _, _, _, _, _, _) -> Error x
            | (_, _, Error x, _, _, _, _, _, _, _) -> Error x
            | (_, _, _, Error x, _, _, _, _, _, _) -> Error x
            | (_, _, _, _, Error x, _, _, _, _, _) -> Error x
            | (_, _, _, _, _, Error x, _, _, _, _) -> Error x
            | (_, _, _, _, _, _, Error x, _, _, _) -> Error x
            | (_, _, _, _, _, _, _, Error x, _, _) -> Error x
            | (_, _, _, _, _, _, _, _, Error x, _) -> Error x
            | (_, _, _, _, _, _, _, _, _, Error x) -> Error x


    let mapError func result =
      match result with
      | Ok value -> Ok value
      | Error error -> func error |> Error


    let withDefault def result =
      match result with
      | Ok value -> value
      | Error _ -> def


    let mapErrorWithDefault def func =
      mapError func >> withDefault def


    let tryWith f x =
      try
        f x |> Ok
      with
      | error -> error |> string |> Error


    let withError result =
      match result with
      | Ok value -> value
      | Error message -> message


    let unpack errFunc okFunc result =
        match result with
            | Error error -> errFunc error
            | Ok ok -> okFunc ok


    let call resultFunc argument =
        match resultFunc with
            | Error message -> Error message
            | Ok func -> func argument |> Ok


    let combine list =
        Seq.fold (map2 (fun s -> Seq.singleton >> Seq.append s))
            (Ok Seq.empty)
            list


    let combineArray array =
        Array.fold (map2 (fun s e -> [| e |] |> Array.append s))
            (Ok Array.empty)
            array


    let combineList list =
        Ok List.empty |> List.foldBack (map2 (fun e l -> e::l)) list


    let flatCombine list =
        list |> List.map (andThen id) |> combine


    type ResultBuilder() =
        member this.Bind(m, f) = andThen f m
        member this.Return(x) = Ok x
        member this.ReturnFrom(m) = m


    let fromChoice choice =
        match choice with
            | Choice1Of2 ok -> Ok ok
            | Choice2Of2 error -> Error error


    let fromOption error option =
        match option with
            | None -> Error error
            | Some x -> Ok x


    let tryFromOption e f x =
        try f x |> fromOption e with | x -> Error x.Message


    let mapBoth errFunc okFunc result =
        match result with
            | Error e -> errFunc e |> Error
            | Ok x -> okFunc x |> Ok


    let combineMap map =
        map
            |> Map.fold (fun s k r -> map2 (Map.add k) r s)
                (Ok Map.empty)

    let unwrap defaultValue okFunc result =
        match result with
            | Error _ -> defaultValue
            | Ok ok -> okFunc ok


    let optionFromOptionResult optionResult =
        optionResult
            |> unpack (Error >> Some)
                (Option.Extra.unwrap None (Ok >> Some))


[<AutoOpen>]
module ResultAutoOpen =
    let result = Result.ResultBuilder ()


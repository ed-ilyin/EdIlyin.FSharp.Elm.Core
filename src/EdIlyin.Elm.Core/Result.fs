namespace EdIlyin.Elm.Core


type Result<'error,'value> =
    | Ok of 'value
    | Err of 'error


module Result =
    let andThen func result =
        match result with
            | Ok value -> func value
            | Err message -> Err message


    let bind = andThen


    let map func ra =
        match ra with
            | Ok a -> Ok (func a)
            | Err x -> Err x


    let map2 func ra rb =
        match (ra,rb) with
            | (Ok a, Ok b) -> Ok (func a b)
            | (Err x, _) -> Err x
            | (_, Err x) -> Err x


    let map3 func ra rb rc =
        match (ra,rb,rc) with
            | (Ok a, Ok b, Ok c) -> Ok (func a b c)
            | (Err x, _, _) -> Err x
            | (_, Err x, _) -> Err x
            | (_, _, Err x) -> Err x


    let map4 func ra rb rc rd =
        match (ra,rb,rc,rd) with
            | (Ok a, Ok b, Ok c, Ok d) -> Ok (func a b c d)
            | (Err x, _, _, _) -> Err x
            | (_, Err x, _, _) -> Err x
            | (_, _, Err x, _) -> Err x
            | (_, _, _, Err x) -> Err x


    let map5 func ra rb rc rd re =
        match (ra,rb,rc,rd,re) with
            | (Ok a, Ok b, Ok c, Ok d, Ok e) -> Ok (func a b c d e)
            | (Err x, _, _, _, _) -> Err x
            | (_, Err x, _, _, _) -> Err x
            | (_, _, Err x, _, _) -> Err x
            | (_, _, _, Err x, _) -> Err x
            | (_, _, _, _, Err x) -> Err x


    let map6 func ra rb rc rd re rf =
        match (ra,rb,rc,rd,re,rf) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f) -> Ok (func a b c d e f)
            | (Err x, _, _, _, _, _) -> Err x
            | (_, Err x, _, _, _, _) -> Err x
            | (_, _, Err x, _, _, _) -> Err x
            | (_, _, _, Err x, _, _) -> Err x
            | (_, _, _, _, Err x, _) -> Err x
            | (_, _, _, _, _, Err x) -> Err x


    let map8 func ra rb rc rd re rf rg rh =
        match (ra,rb,rc,rd,re,rf,rg,rh) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h) -> Ok (func a b c d e f g h)
            | (Err x, _, _, _, _, _, _, _) -> Err x
            | (_, Err x, _, _, _, _, _, _) -> Err x
            | (_, _, Err x, _, _, _, _, _) -> Err x
            | (_, _, _, Err x, _, _, _, _) -> Err x
            | (_, _, _, _, Err x, _, _, _) -> Err x
            | (_, _, _, _, _, Err x, _, _) -> Err x
            | (_, _, _, _, _, _, Err x, _) -> Err x
            | (_, _, _, _, _, _, _, Err x) -> Err x


    let map9 func ra rb rc rd re rf rg rh ri =
        match (ra,rb,rc,rd,re,rf,rg,rh,ri) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h,Ok i) -> Ok (func a b c d e f g h i)
            | (Err x, _, _, _, _, _, _, _, _) -> Err x
            | (_, Err x, _, _, _, _, _, _, _) -> Err x
            | (_, _, Err x, _, _, _, _, _, _) -> Err x
            | (_, _, _, Err x, _, _, _, _, _) -> Err x
            | (_, _, _, _, Err x, _, _, _, _) -> Err x
            | (_, _, _, _, _, Err x, _, _, _) -> Err x
            | (_, _, _, _, _, _, Err x, _, _) -> Err x
            | (_, _, _, _, _, _, _, Err x, _) -> Err x
            | (_, _, _, _, _, _, _, _, Err x) -> Err x


    let map10 func ra rb rc rd re rf rg rh ri rj =
        match (ra,rb,rc,rd,re,rf,rg,rh,ri,rj) with
            | (Ok a,Ok b,Ok c,Ok d,Ok e,Ok f,Ok g,Ok h,Ok i,Ok j) -> Ok (func a b c d e f g h i j)
            | (Err x, _, _, _, _, _, _, _, _, _) -> Err x
            | (_, Err x, _, _, _, _, _, _, _, _) -> Err x
            | (_, _, Err x, _, _, _, _, _, _, _) -> Err x
            | (_, _, _, Err x, _, _, _, _, _, _) -> Err x
            | (_, _, _, _, Err x, _, _, _, _, _) -> Err x
            | (_, _, _, _, _, Err x, _, _, _, _) -> Err x
            | (_, _, _, _, _, _, Err x, _, _, _) -> Err x
            | (_, _, _, _, _, _, _, Err x, _, _) -> Err x
            | (_, _, _, _, _, _, _, _, Err x, _) -> Err x
            | (_, _, _, _, _, _, _, _, _, Err x) -> Err x


    let mapError func result =
      match result with
      | Ok value -> Ok value
      | Err error -> func error |> Err


    let withDefault def result =
      match result with
      | Ok value -> value
      | Err _ -> def


    let mapErrorWithDefault def func =
      mapError func >> withDefault def


    let tryWith f x =
      try
        f x |> Ok
      with
      | error -> error |> string |> Err


    let withError result =
      match result with
      | Ok value -> value
      | Err message -> message


    let unpack errFunc okFunc result =
        match result with
            | Err err -> errFunc err
            | Ok ok -> okFunc ok


    let call resultFunc argument =
        match resultFunc with
            | Err message -> Err message
            | Ok func -> func argument |> Ok


    let combine list =
        Seq.fold (map2 (fun s -> Seq.singleton >> Seq.append s))
            (Ok Seq.empty)
            list


    let combineArray array =
        Array.fold (map2 (fun s -> Array.singleton >> Array.append s))
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
            | Choice2Of2 err -> Err err


    let fromOption error option =
        match option with
            | None -> Err error
            | Some x -> Ok x


    let tryFromOption e f x =
        try f x |> fromOption e with | x -> Err x.Message


    let mapBoth errFunc okFunc result =
        match result with
            | Err e -> errFunc e |> Err
            | Ok x -> okFunc x |> Ok


    let combineMap map =
        map
            |> Map.fold (fun s k r -> map2 (Map.add k) r s)
                (Ok Map.empty)

    let unwrap defaultValue okFunc result =
        match result with
            | Err _ -> defaultValue
            | Ok ok -> okFunc ok


    let optionFromOptionResult optionResult =
        optionResult
            |> unpack (Err >> Some)
                (Option.Extra.unwrap None (Ok >> Some))


[<AutoOpen>]
module ResultAutoOpen =
    let result = Result.ResultBuilder ()


namespace EdIlyin.Elm.Core.Option

module Extra =
    let unwrap def func option =
        match option with
            | None -> def
            | Some value -> func value


    let withDefault defaultValue option =
        match option with
            | None -> defaultValue
            | Some value -> value

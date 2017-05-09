namespace EdIlyin.FSharp.Elm.Core.Json

open Chiron


module Encode =
    let encode = Chiron.Formatting.Json.format


    let string = function | null -> Null () | s -> String s


    let int (i: int) = decimal i |> Number


    let float (f: float) = decimal f |> Number


    let bool b = Bool b


    let null' = Null ()


    let list l = Array l


    let object' kvl = Map.ofList kvl |> Object

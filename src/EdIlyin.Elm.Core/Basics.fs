namespace EdIlyin.Elm.Core

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
[<AutoOpen>]
module Basics =
    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    let (=>) x y = x, y


    let curry f x y = f (x, y)


    let uncurry f (x, y) = f x y


    let first func x _ = func x


    let second func _ y = func y


    let flip func x y = func y x

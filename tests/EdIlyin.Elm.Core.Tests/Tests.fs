module EdIlyin.Elm.Core.Tests

open EdIlyin.Elm.Core
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
  let result = Basics.curry (fun (x, y) -> x + y) 42 24
  printfn "%i" result
  Assert.AreEqual(66,result)

module TreeTest

open NUnit.Framework
open Tree


[<Test>]
let ``Тест на функцию map в двоичном дереве_01`` () =
  let tr = add (add(add Nil 4) 2) 7
  Assert.AreEqual(map (fun x -> x*x) tr, T(T(Nil, 4,Nil), 16, T(Nil, 49, Nil)))

[<Test>]
let ``Тест на функцию map в двоичном дереве_02`` () =
  let tr = Nil
  Assert.AreEqual(map (fun x -> x*x) tr, Nil)

[<Test>]
let ``Тест на функцию filter в двоичном дереве_01`` () =
  let tr = add (add(add Nil 4) 2) 7
  Assert.AreEqual(filter (fun x -> (x%2 = 0)) tr, T(T(Nil, 2, Nil), 4, Nil))

[<Test>]
let ``Тест на функцию filter в двоичном дереве_02`` () =
  let tr = add (add(add Nil 4) 2) 7
  Assert.AreEqual(filter (fun x -> x>100) tr, Nil)

[<Test>]
let ``Тест на функцию fold в двоичном дереве_01`` () =
  let tr = add (add(add Nil 4) 2) 7
  Assert.AreEqual(fold (*) 1 tr, 56)

[<Test>]
let ``Тест на функцию fold в двоичном дереве_02`` () =
  let tr = add (add(add Nil 4) 2) 7
  Assert.AreEqual(fold min 1000 tr, 2)

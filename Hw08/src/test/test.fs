module test

open Calculation 
open NUnit.Framework

[<TestCase ("3^1^2", Result = "3")>]
[<TestCase ("(3^1)^2", Result = "9")>]
[<TestCase ("1-2-3", Result = "-4")>]
[<TestCase ("1-(2-3)", Result = "2")>]
[<TestCase ("24/12", Result = "2")>]
[<TestCase ("1+999999", Result = "1000000")>]
[<TestCase ("21%2+3^2*(1-4)", Result = "-26")>]
[<TestCase ("1/0", Result = "Деление на ноль невозможно" )>]
[<TestCase ("3^(1^2", Result = "Неправильно расставлены скобки")>]
[<TestCase ("2+2-", Result = "Неверный ввод выражения")>]
let ``Тесты на вычисление выражения`` expr =
  check expr

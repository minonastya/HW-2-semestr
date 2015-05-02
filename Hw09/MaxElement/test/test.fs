module Test
open MaxEl
open NUnit.Framework

[<TestCase ([|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|],1 ,  Result = "10")>]
[<TestCase ([|1; 1; 1; 1; 1; 1; 1; 1; 1; 1|], 2,  Result = "1")>]
[<TestCase ([|2; 4; 5; 23; 4; 34; 5; 34; 144; 45|], 3, Result = "144")>]
[<TestCase ([|-10; -2; -4; -5; -6; -7; -8; -9|], 4, Result = "-2")>]
[<TestCase ([|1; -2; 3; -4; 5; -6; 7; -8; 9; -10|], 5, Result = "9")>]
[<TestCase ([|1; 2|], -2, Result = "Число потоков должно быть больше нуля")>]
let ``Тесты на вычисление выражения`` arr th =
  check  th arr
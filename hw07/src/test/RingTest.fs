module RingTest

open NUnit.Framework
open Rings


[<TestCase(5, Result = 0)>]
[<TestCase(4, Result = 2)>]
[<TestCase(8, Result = 2)>]
let ``Тест на вычисление выражений в кольце вычетов по модулю n_01`` n = 
   ring n
    {
    let! a = 2 * 3
    let! b = 4
    return a + b
    }

[<TestCase(5, Result = 2)>]
[<TestCase(4, Result = 0)>]
[<TestCase(8, Result = 4)>]
let ``Тест на вычисление выражений в кольце вычетов по модулю n c с отрицательными числами_02`` n = 
   ring n
    {
    let! a = -3
    let! b = -4
    return a * b
    }


[<TestCase(3, Result = 0)>]
[<TestCase(6, Result = 3)>]
[<TestCase(2, Result = 1)>]
let ``Тест на вычисление выражений в кольце вычетов по модулю n_03`` n = 
   ring n
    {
    let! a = 1
    let! b = 2
    let! c = -3
    let! d = -4
    return a + b - c * d
    }

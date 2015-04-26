module Rings
//Задание 40
//Ожидаемое время выполнения - 2 часа, реальное - 2,5 часа

open Tree

type RingsBuilder (n) =
  let modul x = 
    if (x % n) < 0 then (x % n) + n
    else x % n
  member this.Bind(x, f) = f(modul x)
  member this.Return x = modul x

let ring n = new RingsBuilder(n)




[<EntryPoint>]
let main argv =
  printfn "Задание 40"
  printfn "Вычислим выражение a + b в кольце вычетов по модулю 5:"
  let compute = 
    ring 5
      {
      let! a = 2 * 3
      let! b = 4
      return a + b
      }
  printfn "(a = 2 * 3)"
  printfn "(b = 4)"
  printfn "(a + b) mod 5 = %d" compute
  printfn ""
  printfn ""
  printfn "Задание 41"
  let tr = T(Nil, 3, T(T(Nil,4, Nil), 5, T(Nil, 2, T(Nil, 1, Nil))))
  printfn "Первоначальное дерево:"
  printfn "%A" tr
  let tree = map (fun x -> x + 5) tr
  printfn "Увеличим каждый элемент дерева на 5:"
  printfn "%A" tree
  let tree = filter (fun x -> if (x > 7) then true else false) tree
  printfn "Удалим все элементы, не превосходящие 7:"
  printfn "%A" tree
  let tree = fold (+) 0 tree
  printfn "Посчитаем сумму всех элементов дерева:"
  printfn "%A" tree
  0 

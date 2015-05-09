module MaxEl 
open System
open System.Threading

let maxInRange (array: int array) l r = 
  let mutable max = None
  for i in l .. r do
    if Some array.[i] > max then max <- Some array.[i]
  max

(* Не учитывает последние элементы массива, если количество потоков не кратно размеру массива
val array : int [] = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
> maxSearch 2 array;;
val it : int option = Some 10
> maxSearch 3 array;;
val it : int option = Some 9 
  Не рассмотрены случаи некорректного ввода числа потоков и пустого массива*)

let maxSearch_bad threadNum (array: int []) = 
  let size = Array.length array
  let step = if threadNum < size then (size / threadNum)
             else 
               let threadNum = size
               1
  let max = ref None
  let threadArray = Array.init threadNum (fun e ->
    new Thread(ThreadStart(fun _ -> 
         let threadMax = maxInRange array (e * step) ((e + 1) * step - 1)
         Monitor.Enter(max)
         if threadMax > !max then max := threadMax
         Monitor.Exit(max) ))
   )
  for t in threadArray do
        t.Start()
  for t in threadArray do
        t.Join()
  max.Value



(*Исправлены недочеты
Делаем проверку: если поток последний и размер массива кратен числу потоков, 
то в последним потоком проходим до конца массива.
Обработаны ошибки. *)

let maxSearch threadNum array = 
  let size = Array.length array  
  if threadNum <= 0 then failwith "Число потоков должно быть больше нуля"   
  elif array = [||] then failwith "Массив пуст"
  else 
    let step = if threadNum < size then (size / threadNum)
               else 
                  let threadNum = size
                  1
    let max = ref None
    let threadArray = Array.init threadNum (fun e ->
      new Thread(ThreadStart(fun _ -> 
        let mutable threadMax = None
        if (e = threadNum - 1)&&(size % threadNum <> 0) then
          threadMax <- maxInRange array (e * step) (size - 1)
        else
          threadMax <- maxInRange array (e * step) ((e + 1) * step - 1)
        Monitor.Enter(max)
        if threadMax > !max then max := threadMax
        Monitor.Exit(max) ))
     )
    for t in threadArray do
          t.Start()
    for t in threadArray do
          t.Join()
    max.Value


let check th ar =
  try  
    ((maxSearch th ar).Value.ToString())
  with
  |Failure msg ->  msg

let duration p s = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let rnd = new System.Random(0)
    let temp = maxSearch p (Array.init s (fun _ -> rnd.Next(0, s)))
    printfn "Threads: %d\t Size: %d\t Time: %A" p s timer.Elapsed


[<EntryPoint>]
let main argv = 
    let array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
    printfn "Массив: %A" array
    printfn "Количество потоков: %d" 5
    printfn "Максимальный элемент: %s" (check 5 array)
    let array = [|1; 2; 3; 4; 5|]
    printfn "Массив: %A" array
    printfn "Количество потоков: %d" -2
    printfn "Максимальный элемент: %s" (check -2 array)
    let array = [||]
    printfn "Массив: %A" array
    printfn "Количество потоков: %d" 3
    printfn "Максимальный элемент: %s" (check 3 array)
    0 

//Задачи 35-36: калькулятор мат. выражений/выражений с переменными
// Ожидаемое время выполнения 10 часов - реальное ~ 15 часов

module Operators
open System
open System.IO
open NUnit.Framework


type Stack<'A> = 
  interface
    abstract push : 'A -> unit
    abstract pop  : Option<'A>
    abstract isEmpty : bool
    abstract top  : Option<'A>
    abstract printf : unit
    abstract size : int
    abstract rev : unit
  end

type ListStack<'A> () = 
  class
    let mutable list = []
    interface Stack<'A> with 
      member s.push a = list <- a ::list
      member s.isEmpty = list.IsEmpty 
      member s.top = 
        if list.IsEmpty then None
        else Some list.Head 
      member s.pop  = 
        if list.IsEmpty then 
          None
        else 
          let head = list.Head
          list <- list.Tail
          Some head
      member s.printf =
        let rec print list =
          match list with
          |[] -> ()
          |a :: b -> printf "%A " a
                     print b
        list <- List.rev list
        print list
        printfn ""
      member s.size =
        let rec size list count =
          match list with 
          |[] -> count
          |a :: b -> size b (count + 1)
        size list 0
      member s.rev = list <- List.rev list                  
  end


let postfix (expr: string) =
  
  let mutable tokens = []
  let mutable lexema = ""
  for i in 0..expr.Length - 1 do
    match expr.[i] with
    |'-' -> if expr.[i-1] = '(' then lexema <- "-"
            else 
              if System.Char.IsDigit(expr.[i-1]) then 
                   tokens <- List.append tokens [lexema]
                   lexema <- ""
              tokens <- List.append tokens ["-"]
    |'(' -> tokens <- List.append tokens ["("]
    |')' -> tokens <- List.append tokens [lexema]
            lexema <- ""
            tokens <- List.append tokens [")"]
    |' ' -> if System.Char.IsDigit(expr.[i-1]) then
              tokens <- List.append tokens [lexema]
              lexema <- ""
    | _ -> if System.Char.IsDigit(expr.[i]) then lexema <- lexema + expr.[i].ToString()
           else 
             if (lexema <> "")&&(System.Char.IsDigit(expr.[i-1])) then 
                  tokens <- List.append tokens [lexema]
                  lexema <- ""
             tokens <- List.append tokens [expr.[i].ToString()]
                
  if lexema.Length <> 0 then tokens <- List.append tokens [lexema]


  let prior op = 
      match op with
      |"^" -> 3
      |"*" -> 2
      |"/" -> 2
      |"%" -> 2
      |"+" -> 1
      |"-" -> 1
      | _  -> 0
  let op x = (prior x <> 0)
  
  let stackOut = new ListStack<string>():> Stack<string>
  let stackOp = new ListStack<string>():> Stack<string>
  let rec f t =
    match t with
    |[] -> while not (stackOp.isEmpty) do
             if stackOp.top = Some ")" then
               while stackOp.top <> Some "(" do 
                 (stackOut.push (stackOp.top.Value))       
                 ignore(stackOp.pop)     
               ignore(stackOp.pop)
             else
               stackOut.push (stackOp.top.Value)
               ignore(stackOp.pop)             
    |x :: y -> match x with
               |"(" -> stackOp.push ("(")
               |")" -> while stackOp.top.Value <> "(" do 
                         stackOut.push (stackOp.top.Value)
                         ignore(stackOp.pop)
                       ignore(stackOp.pop)
               | _ -> if Char.IsLetter x.[0]  then stackOut.push x
                      if (x.Length > 1)&&(not (System.Char.IsDigit(x.[0]))) then 
                        stackOut.push x
                      elif (x.Length > 0)&&(System.Char.IsDigit(x.[0])) then stackOut.push x
                      elif op x then 
                        if stackOp.isEmpty then ignore(stackOp.push(x))
                        elif (prior x <= (prior stackOp.top.Value))&&(prior x < 3) then 
                          (stackOut.push (stackOp.top.Value))       
                          ignore(stackOp.pop)
                          stackOp.push(x)
                        elif (prior x = 3) then 
                          stackOp.push(x)
                        else stackOp.push(x)
               f y                                                          
  f tokens
  stackOut



let stackmachine (expr: string) (values: char array) (context: int array) =
  let stackOut = new ListStack<string>():> Stack<string>
  let stackVal = new ListStack<int>():> Stack<int>
  let stackOut = postfix expr
  stackOut.rev
  while not(stackOut.isEmpty) do
    let token = stackOut.top.Value
    ignore(stackOut.pop)
    if System.Char.IsLetter token.[0] then stackVal.push context.[(Array.tryFindIndex (fun x -> x = token.[0]) values).Value]  
    elif (token.Length > 1)&&(not (System.Char.IsDigit(token.[0]))) then stackVal.push (System.Convert.ToInt32(token))
    elif System.Char.IsDigit(token.[0]) then stackVal.push (System.Convert.ToInt32(token))
    else 
      if stackVal.size > 1 then
        let x = stackVal.top.Value
        ignore(stackVal.pop)
        let y = stackVal.top.Value
        ignore(stackVal.pop)
        match token with
            |"^" -> stackVal.push (pown y x)
            |"*" -> stackVal.push (y * x)
            |"/" -> stackVal.push (y / x)
            |"%" -> stackVal.push (y % x)
            |"+" -> stackVal.push (y + x)
            |"-" -> stackVal.push (y - x)
            |_ -> () 
  stackVal.top.Value

[<TestCase ("3 ^ 1 ^ 2", Result = 3)>]
[<TestCase ("(3 ^ 1) ^ 2", Result = 9)>]
[<TestCase ("1 - 2 - 3", Result = -4)>]
[<TestCase ("1 - (2 - 3)", Result = 2)>]
[<TestCase ("24 / 12", Result = 2)>]
[<TestCase ("1+999999", Result = 1000000)>]
[<TestCase ("21 % 2 + 3 ^ 2 *(1 - 4)", Result = -26)>]
let ``Тесты на вычисление выражения`` expr =
  stackmachine expr [||] [||]

[<TestCase ("x + 3 - 2", [|'x'|], [|4|], Result = 5)>]
[<TestCase ("x ^ y ^ z", [|'x'; 'y'; 'z'|], [|3; 1; 2|], Result = 3)>]
let ``Тесты на вычисление выражения с переменными`` expr ar1 ar2 = 
  stackmachine expr ar1 ar2

  

[<EntryPoint>]
let main argv = 
  printfn "Выражение: (1 + 2) ^ 3 * 4 / 5 - 6"
  let x = "(1 + 2) ^ 3 * 4 / 5 - 6"
  printfn"Перевод выражения в постфиксную запись: "
  (postfix x).printf
  printf "Значения выражения: "
  let y = stackmachine x [||] [||]
  printfn "%d" y
  printfn""
  printfn "Выражение: x ^ 2 + (x - 3) / (x + 1)"
  let a = "x ^ 2 + (x - 3) / (x + 1)"
  printfn"Перевод выражения в постфиксную запись: "
  (postfix a).printf
  printf "Значения выражения: "
  let y = stackmachine a [|'x'|] [|1|]
  printfn "%d" y
  0 // возвращение целочисленного кода выхода

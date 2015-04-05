//37-38 задания
//Время выполнения: ожидаемое - 6 часов, реально ~ 10 часов
open System
open System.IO
open NUnit.Framework

type Stack<'A> = 
  interface
    abstract push : 'A -> unit
    abstract pop  : Option<'A>
    abstract isEmpty : bool
    abstract top  : Option<'A>
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
  end

let postfix () =
  use instream = new StreamReader("input.txt")
  use outstream = new StreamWriter("output.txt")
  let expr = instream.ReadToEnd()
  
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
             if System.Char.IsDigit(expr.[i-1]) then 
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
  
  let stack = new ListStack<string>():> Stack<string>
  let rec f t =
    match t with
    |[] -> while not (stack.isEmpty) do
             if stack.top = Some ")" then
               while stack.top <> Some "(" do 
                 outstream.WriteLine(stack.pop.Value)               
               ignore(stack.pop)
             else
               while not stack.isEmpty do 
                 outstream.WriteLine(stack.pop.Value)               
    |x :: y -> match x with
               |"(" -> stack.push ("(")
               |")" -> while stack.top.Value <> "(" do 
                         outstream.WriteLine(stack.pop.Value)
                       ignore(stack.pop)
               | _ -> if (x.Length > 1)&&(System.Char.IsDigit(x.[1])) then outstream.WriteLine(x)
                      elif (x.Length > 0)&&(System.Char.IsDigit(x.[0])) then outstream.WriteLine(x)
                      elif op x then 
                        if stack.isEmpty then ignore(stack.push(x))
                        elif prior x <= (prior stack.top.Value) then 
                          outstream.WriteLine(stack.top.Value)
                          ignore(stack.pop)
                          stack.push x
                        else stack.push x
               f y                                                          
  f tokens

let (^) a b =
    let mutable temp = 1
    for i in 1..b do
        temp <- temp*a
    temp

let stackmachine () =
   use instream = new StreamReader("output.txt")
   use outstream = new StreamWriter("output2.txt")

   let stack = new ListStack<int>():> Stack<int>
   while not instream.EndOfStream do
     let expr = instream.ReadLine()
     if expr.Length > 0 then
       if (expr.Length > 1)&&(not (System.Char.IsDigit(expr.[0]))) then stack.push ((-1)*Convert.ToInt32(expr.[1..expr.Length - 1]))
       elif System.Char.IsDigit(expr.[0]) then stack.push (Convert.ToInt32(expr))
       else 
         let x = stack.pop.Value
         let y = stack.pop.Value
         match expr with
         |"^" -> stack.push(y ^ x)
         |"*" -> stack.push(x * y)
         |"/" -> stack.push(y / x)
         |"%" -> stack.push(y % x)
         |"+" -> stack.push(x + y)
         |"-" -> stack.push(y - x)
         |_ -> ()

   outstream.WriteLine(stack.pop.Value)

let write1 (str : string) =
    use stream = new StreamWriter("input.txt")
    stream.WriteLine(str)

let read1 =
    use stream = new StreamReader("output.txt")
    stream.ReadToEnd()

let write2 (str : string) = 
    use stream = new StreamWriter("output.txt")
    stream.WriteLine(str)
let read2 =
    use stream = new StreamReader("output2.txt")
    stream.ReadToEnd()

[<TestCase ("(-2) * 5", Result = "-10")>]
[<TestCase ("1+999999", Result = "1000000")>]
[<TestCase ("3 + 4 * 2 / (1 - 5)^2", Result = "3")>]
[<TestCase ("1+2*4", Result = "9")>]
[<TestCase ("5-2+(4-1*4)^0", Result = "4")>]
[<TestCase ("21 % 2 + 3 ^ 2 *(1 - 4)", Result = "-26")>]
let ``Тесты`` expr =
  write1(expr)
  postfix()
  write2(read1)
  stackmachine()
  (read2).TrimEnd('\r', '\n')

  

[<EntryPoint>]
let main argv = 
    0 // возвращение целочисленного кода выхода

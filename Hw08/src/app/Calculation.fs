module Calculation

open System
//Реализация стека
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

//Проверки на неправильный ввод
let divByZero x y =
  if y = 0 then failwith "Деление на ноль невозможно"
  else x / y

let checkBrackets x = 
  if x <> 0 then failwith "Неправильно расставлены скобки" 

let checkExpr x = 
  if (x <> 0) && (x <> 1) then failwith "Неверный ввод выражения"

//Перевод из постфиксной записи в инфиксную
let postfix (expr: string) =
  let mutable count = 0
  let mutable bracks = 0
  let mutable tokens = []
  let mutable lexema = ""
  for i in 0..expr.Length - 1 do
    match expr.[i] with
    |'-' -> if (i = 0) then lexema <- "-"
            elif expr.[i-1] = '(' then lexema <- "-"
            else 
              if System.Char.IsDigit(expr.[i-1]) then 
                   tokens <- List.append tokens [lexema]
                   lexema <- ""
                   count <- count + 1
              tokens <- List.append tokens ["-"]; count <- count - 1
    |'(' -> tokens <- List.append tokens ["("]; bracks <- bracks + 1
    |')' -> tokens <- List.append tokens [lexema]
            if lexema <> "" then count <- count + 1
            lexema <- ""
            tokens <- List.append tokens [")"]
            bracks <- bracks - 1
    | _ -> if System.Char.IsDigit(expr.[i]) then lexema <- lexema + expr.[i].ToString()
           else 
             if (lexema <> "")&&(System.Char.IsDigit(expr.[i-1])) then 
                  tokens <- List.append tokens [lexema]
                  lexema <- ""
                  count <- count + 1
             tokens <- List.append tokens [expr.[i].ToString()]; count <- count - 1
    checkExpr count                
  if lexema.Length <> 0 then tokens <- List.append tokens [lexema]; count <- count + 1
  if count <> 1 then failwith "Неверный ввод выражения"
  checkBrackets bracks 


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

//Вычисление значения выражения в инфиксной записи
let stackmachine (expr: string) =
  let stackOut = new ListStack<string>():> Stack<string>
  let stackVal = new ListStack<int>():> Stack<int>
  let stackOut = postfix expr
  stackOut.rev
  while not(stackOut.isEmpty) do
    let token = stackOut.top.Value
    ignore(stackOut.pop)
    if (token.Length > 1)&&(not (System.Char.IsDigit(token.[0]))) then stackVal.push (System.Convert.ToInt32(token))
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
            |"/" -> stackVal.push (divByZero y x)
            |"%" -> stackVal.push (y % x)
            |"+" -> stackVal.push (y + x)
            |"-" -> stackVal.push (y - x)
            |_ -> () 
  stackVal.top.Value

let check ex =
  try 
    (stackmachine ex).ToString()
  with
  | Failure msg -> msg
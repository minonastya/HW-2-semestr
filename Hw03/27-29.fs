type IList<'A> = 
  interface
    abstract AddTop: 'A -> unit
    abstract AddBack: 'A -> unit
    abstract AddByNumb: 'A -> int -> unit
    abstract DelTop: unit
    abstract DelBack: unit
    abstract DelByNumb: int -> unit
    abstract Search: ('A -> bool) -> Option<'A>
    abstract Printf: unit
    abstract Top: unit ->'A
    abstract Empty: bool
    abstract Concat: IList<'A> -> unit
  end

type List<'A when 'A: equality> = Empty|Cons of 'A * List<'A>

type ATDList<'A when 'A: equality>(list: List<'A>) =
  class
    let mutable l = list
    interface IList<'A> with
      member self.AddTop el = l <- Cons(el, l)

      member self.AddBack el =
        let rec addBack l =
          match l with 
          |Empty -> Cons(el, Empty)
          |Cons(x, y) -> Cons(x, addBack y)
        l <- addBack l

      member self.AddByNumb el n =
        let rec addNumb l x =
          match l, x with
          |Empty, _ -> failwith "Not enough elements!"
          |Cons(a,b), x -> if x < n then Cons(a, addNumb b (x+1))
                           elif x = n then Cons(el, l)
                           else failwith "Not enough elements!"
        l <- addNumb l 1
      
      member self.DelTop =
        match l with
        |Empty -> failwith "Not enough elements!"
        |Cons(x, y) -> l <- y

      member self.DelBack =
        let rec delBack l =
          match l with 
          |Empty -> failwith "Not enough elements!"
          |Cons(x, Empty) -> Empty
          |Cons(x, y) -> Cons(x, delBack y)
        l <- delBack l

      member self.DelByNumb n =
        let rec delNumb l x =
          match l with
          |Empty -> failwith "Not enough elements!"
          |Cons(a, b) -> if x < n then Cons(a, delNumb b (x+1))
                         elif x = n then  b
                         else failwith "Not enough elements!"
        l <- delNumb l 1

      member self.Search el =
        let rec search l =
          match l with
          | Empty -> None
          | Cons(a, b) -> if el a then Some(a) 
                          else (search b)
        search l
        
      member self.Printf = 
        let rec print l =
          match l with
          | Empty -> ()
          | Cons(a, b) -> printf "%A " a
                          print b
        print l
        printfn ""
        
      member self.Top() = 
        match l with
        |Empty -> failwith "Not enough elements!"
        |Cons(a,b) -> a

      member self.Empty  = if l = Empty then true 
                           else false
      member self.Concat L =
        let rec add l el =
          match l with
          | Empty -> el
          | Cons (x, y) -> Cons (x, add y el)
        let mutable r = Empty
        while L.Empty = false do
          r <- add r (Cons (L.Top(), Empty))
          L.DelTop
        l <- add l r
                
  end
        

type ArrList<'A when 'A: equality>(array : 'A []) =
  class  
    let mutable arr = array 
  
    interface IList<'A> with
      member self.AddTop el = arr <- Array.append [|el|] arr
    
      member self.AddBack el = arr <- Array.append arr [|el|]
    
      member self.AddByNumb el n =
        if (n <= 0) || (n = arr.Length+1) then failwith "Not enough elements!"
        elif n =  1 then arr <- Array.append [|el|] arr
        elif n = arr.Length then arr <- Array.append arr [|el|]
        else arr <- Array.append (Array.append arr.[0..(n-2)] [|el|]) arr.[n-1..(arr.Length-1)]
    
      member self.DelTop = arr <- arr.[1..(arr.Length - 1)]
    
      member self.DelBack = arr <- arr.[0..(arr.Length - 2)]
    
      member self.DelByNumb n =
        match arr with 
        | [||] -> failwith "Not enough elements!"
        | _  -> if n = (arr.Length) + 1 then arr <- arr.[0..(arr.Length - 2)]
                elif n < 0 || n > arr.Length then failwith "Not enough elements!"
                else arr <- Array.append arr.[0..(n - 2)] arr.[n..(arr.Length - 1)]
    
      member self.Search el = Array.tryFind el arr
    
      member self.Top() = arr.[0]

      member self.Empty = if arr = [||] then true
                          else false
    
      member self.Printf = printf "%A\n" arr 
    
      member self.Concat L = 
                  while L.Empty = false do
                    arr <- Array.append arr [|L.Top();|]
                    L.DelTop
 end  
 
let output (list: IList<int>) (list2: IList<int>) =   
  printf "Список: " 
  list.Printf
  printfn ""
  printfn "Добавим в начало списка 0: "
  list.AddTop 0
  list.Printf
  printfn "Добавим в конец списка -5: "
  list.AddBack -5
  list.Printf
  printfn "Добавим на 5 место число 8: "
  list.AddByNumb 8 5
  list.Printf
  printfn "Удалим начало списка: "
  list.DelTop
  list.Printf
  printfn "Удалим конец списка: "
  list.DelBack
  list.Printf
  printfn "Удалим третий элемент списка: "
  list.DelByNumb 3
  list.Printf   
  printfn "Выведем первый элемент, больше 8: "
  let l = list.Search (fun x -> (x > 8))
  match l with
  |None -> printfn "Таких элементов в списке нет"
  |Some a -> printfn "%A" a 
  printfn "2 список: "
  list2.Printf
  printfn "Соединим первый и второй списки: "
  list.Concat list2
  list.Printf

[<EntryPoint>]
let main argv =
  let list = (Cons(2, Cons(9, Cons(5, Cons(1, Cons(7, Empty))))))
  let newList = new ATDList<int> (list)  
  let List = newList :> IList<int>
  let list2 = Cons(0, Cons(1, Cons(2, Cons(3, Empty))))
  let newList2 = new ATDList<int> (list2)
  let List2 = newList2 :> IList<int>
  printfn "Реализация списка на АТД: "
  output List List2
  printfn ""
  let array = [|2; 9; 5; 1; 7|]
  let newArray = new ArrList<int> (array)
  let Arr = newArray :> IList<int>
  let array2 = [|0; 1; 2; 3|]
  let newArray2 = new ArrList<int> (array2)
  let Arr2 = newArray2 :> IList<int>
  printfn "Реализация списка на массиве: "
  output Arr Arr2
  0
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
          |_, 1 -> Cons(el, l)
          |Empty, _ -> failwith "Not enough elements!"
          |Cons(a,b), x -> if x < n then Cons(a, addNumb b (x+1))
                           elif x = n then Cons(el, l)
                           else failwith "Not enough elements!"
        l <- addNumb l n
      
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
        l <- delNumb l n

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
        if (n < 0) || (n = arr.Length) then failwith "Not enough elements!"
        elif n =  0 then arr <- Array.append [|el|] arr
        elif n = arr.Length then arr <- Array.append arr [|el|]
        else arr <- Array.append (Array.append arr.[0..(n-1)] [|el|]) arr.[n..(arr.Length-1)]
    
      member self.DelTop = arr <- arr.[1..(arr.Length - 1)]
    
      member self.DelBack = arr <- arr.[0..(arr.Length - 2)]
    
      member self.DelByNumb n =
        match arr with 
        | [||] -> failwith "Not enough elements!"
        | _  -> if n = 0 then arr <- arr.[1..(arr.Length - 1)]
                elif n = arr.Length then arr <- arr.[0..(arr.Length - 2)]
                elif n < 0 || n > arr.Length then failwith "Not enough elements!"
                else arr <- Array.append arr.[0..(n - 1)] arr.[(n + 1)..(arr.Length - 1)]
    
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
 
                
  


[<EntryPoint>]
let main argv = 
  0
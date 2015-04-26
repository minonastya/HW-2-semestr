module Tree
//Задание 41
//Ожидаемое время выполнения - 3 часа, реальное - 4 часа

type Tree<'A> = Nil|T of Tree<'A> * int * Tree<'A>


let rec add tr v = 
  match tr with
  |Nil -> T(Nil, v, Nil)
  |T(l, x, r) -> if (v > x) then T(l, x, (add r v))
                   elif (v < x) then T((add l v), x, r)
                   else T(l, x, r)
let node tr = 
  match tr with 
  |T(l,x,r) -> x
  |_ -> failwith "Nil"

let rec merge a b =
    match b with
    |Nil -> a
    |T(l,x,r) -> merge (merge (add a (node b)) l) r
  
type TreeBuilder () =
  member this.Bind(x, f) = 
    match x with
    |Nil -> Nil
    |T(l, c, r) -> T(this.Bind(l,f), f c, this.Bind(r,f))
  member this.Return x = x
  member this.Combine(a, b) = merge a b
  member this.Delay f = f()
  member this.For(x, f) = this.Bind(x,f)
                       

let tree = new TreeBuilder()

let rec fold f acc tr =
  tree {
     match tr with
     |Nil -> return acc
     |T(l,x,r) -> let b = f acc x
                  return fold f (fold f b l) r
    }

let rec filter f tr =
  tree {
     match tr with
     |Nil -> return Nil
     |T(l,x,r) -> if (f x) then return T(filter f l, x, filter f r)
                  else return filter f r
                       return filter f l
    }

let map f tr =
  tree {
    for i in tr do
      return f i
  } 


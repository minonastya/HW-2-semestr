//Ожидаемое время выполнения - 1,5 часа, реальное - 2,5 часа
type Tree = Nil | T of Tree * int * Tree

let rec add v tr = 
  match tr with
  |Nil -> T(Nil, v, Nil)
  |T(left, x, right) -> if (v > x) then T(left, x, (add v right))
                        else if (v <x) then T((add v left), x, right)
                        else T(left, x, right)

let rNumb tr = //значение в правом узле
  match tr with
  |Nil -> 0
  |T(left,x,right) ->x

let rec del v tr =
  match tr with
  |Nil -> Nil
  |T(left, x, right) -> if (v > x) then T(left, x, (del v right))
                        else if (v < x) then T((del v left), x, right)
                        else match right with
                             |Nil -> left
                             |right -> if (left = Nil) then right
                                       else T(left, rNumb right, (del (rNumb right) right))

let rec LCR tr =
  match tr with
  |Nil -> printf ""
  |T(left, x, right) -> LCR left
                        printf"%d " x
                        LCR right
               
let rec LRC tr =
  match tr with
  |Nil -> printf ""
  |T(left, x, right) -> LRC left
                        LRC right
                        printf"%d " x
                       

let rec CLR tr =
  match tr with
  |Nil -> printf ""
  |T(left, x, right) -> printf"%d " x
                        CLR left
                        CLR right
                        
[<EntryPoint>]
let main argv = 
    let tree = (add 3( add 10(add 1( add 7(add 4 Nil)))))
    LRC tree
    printf"\n"
    CLR tree
    printf"\n"
    LCR tree
    printf"\n"
    printf"\n"
    let tree = (add 8(add 20(add 2(add 16(add 12 tree)))))
    LRC tree
    printf"\n"
    CLR tree
    printf"\n"
    LCR tree
    printf"\n"
    printf"\n"
    let tree = (del 0(del 4(del 16(del 5 tree))))
    LRC tree
    printf"\n"
    CLR tree
    printf"\n"
    LCR tree
    printf"\n"
    0 

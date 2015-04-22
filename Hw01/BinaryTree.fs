//Ожидаемое время выполнения - 1,5 часа, реальное - 2,5 часа
type Tree = Nil | T of Tree * int * Tree

let rec add v tr = 
  match tr with
  |Nil -> T(Nil, v, Nil)
  |T(left, x, right) -> if (v > x) then T(left, x, (add v right))
                        else if (v <x) then T((add v left), x, right)
                        else T(left, x, right)

let rec rInLeft tr = //значение в самом правом узле левого поддерева
  match tr with
  |Nil -> 0
  |T(left, x, Nil)-> x
  |T(left, x, right) -> (rInLeft right)

let rec lInRight tr = //значение в самом левом узле правого поддерева
  match tr with
  |Nil -> 0
  |T(Nil, x, right)-> x
  |T(left, x, right) -> (lInRight left)
  

let Numb tr = //значение в узле
  match tr with
  |Nil -> 0
  |T(left,x,right) ->x

let rec del v tr =
  match tr with
  |Nil -> Nil
  |T(left, x, right) -> if (v > x) then T(left, x, (del v right))
                        else if (v < x) then T((del v left), x, right)
                        else match left, right with
                             |Nil, Nil -> Nil
                             |left, Nil -> left
                             |Nil, right -> right
                             |T(l, x, r), right -> if r = Nil then T(left, lInRight right, del (lInRight right) right)
                                                   else T(del (rInLeft left) left, rInLeft left, right )

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
    let tree = (add 4 Nil)
    printf "%A" tree
    printf"\n"
    printf "Insert 7: "
    let tree = (add 7 tree)
    printf "%A" tree
    printf"\n"
    printf "Insert 1: "
    let tree = (add 1 tree)
    printf "%A" tree
    printf"\n"
    printf "Insert 5: "
    let tree = (add 5 tree)
    printf "%A" tree
    printf"\n"
    printf "Insert 2: "
    let tree = (add 2 tree)
    printf "%A" tree
    printf"\n"
    printf "Insert 10: "
    let tree = (add 10 tree)
    printf "%A" tree
    printf"\n"
    printf "Delete 7: "
    let tree = (del 7 tree)
    printf "%A" tree
    printf"\n"
    printf "Delete 4: "
    let tree = (del 4 tree)
    printf "%A" tree
    printf"\n"
    LCR tree
    printf"\n"
    LRC tree
    printf"\n"
    CLR tree
    0 

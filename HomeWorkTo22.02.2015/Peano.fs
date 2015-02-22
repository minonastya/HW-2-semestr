type Peano = Zero|S of Peano

let suc(p:Peano) = S p 

let minus1(p:Peano) = 
  match p with
  |Zero -> Zero
  |S p -> p

let rec plus a b =
  match a, b with
  |Zero, Zero -> Zero
  |S a, Zero -> S a
  |Zero, S b -> S b
  |S a, S b -> S( S(plus a b))

let rec minus a b =
  match a, b with
  |Zero, Zero -> Zero
  |S a, Zero -> S a
  |Zero, S b -> S b
  |S a, S b -> minus a b

let rec toInt a =
  match a with
  |Zero -> 0
  |S a -> 1 + toInt a  

let intToStr a =
  a.ToString()

let printPeano a = a |> toInt
                     |> intToStr
                     |> printf "%s\n"
                     
let rec multi a b =
  match a with
  |Zero -> Zero
  |S a -> plus(multi a b) b
  
let rec exp a b = 
  match b with
  |Zero -> (S(Zero))
  |S b -> multi a (exp a b)


[<EntryPoint>]
let main args = 
    printf "%A\n" (suc(Zero))
    printf "%A\n" (minus1 (S(S(S(S(Zero))))))
    printf "%A\n" (plus (S(S(Zero))) (S(S(S(Zero)))))
    printf "%A\n" (minus (S(S(S(Zero)))) (S(S(S(Zero)))))
    printf "%A\n" (printPeano (S(S(S(S(S(S(S(Zero)))))))))
    printf "%A\n" (multi (S(S(S(Zero)))) (S(S(Zero))))
    printf "%A\n" (exp (S(S(Zero))) (S(S(S(Zero)))))
    0 

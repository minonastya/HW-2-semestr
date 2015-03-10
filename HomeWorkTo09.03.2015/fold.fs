 //Тип List.iter: val it : (('a -> unit) -> 'a list -> unit) 

let reverse l = List.fold (fun list x -> x::list) [] l

let filter f l = List.foldBack (fun x list -> if f x then x::list else list) l []

let map f l = List.foldBack (fun x list -> f x :: list) l [] 

let horner x0 l = List.fold(fun sum a -> x0*sum + a) 0 (reverse l)

[<EntryPoint>]
let main argv = 
  printfn "Тип List.iter: val it : (('a -> unit) -> 'a list -> unit) "
  let data = [ 1; 2; 4; 6; 7 ] 

  printfn"Primary list: "
  data |> List.iter (fun x -> printf "%A " x)
  printfn""

  printfn"Reversed list: "
  reverse data |>List.iter (fun x -> printf "%A " x)
  printfn""

  printfn"Filter list for even numbers: "
  filter (fun x -> x%2 = 0) data|> List.iter (fun x -> printf "%A " x)
  printfn""
  
  printfn"New list with elements increase on 3: "
  map (fun x -> x+3) data|> List.iter (fun x -> printf "%A " x)
  printfn""

  printfn"Realization scheme Horner's with value x=2"
  printfn"y = 1 + 2*x + 4*x^2 + 6*x^3 + 7*x^4"
  printf"y = " 
  printfn"%A"(horner 2 data)
  0 
 

 //Тип List.iter: val it : (('a -> unit) -> 'a list -> unit) 

let reverse l = List.fold (fun x list -> list::x) [] l

let filter f l = List.fold (fun list x -> if f x = true then list @ [x] else list) [] l

let map f l = List.fold (fun list x -> list @[f x]) [] l

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
 

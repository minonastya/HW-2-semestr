open System
open System.Windows.Forms
open System.Drawing
open Calculation

let mutable result = ""


let checkFail expr =
  try
    (stackmachine expr).ToString()   
  with
  |Failure msg -> ignore(MessageBox.Show (msg, "Ошибка!"));""

let calc (but:Button) (lbl:Label) = 
  match but.Text with
  |"=" -> result <- checkFail result        
  |"C" -> result <- ""
  | a -> result <- result + a
  lbl.Text <- result

let Label =
  let lbl = new Label()
  lbl.Location <- Point(100, 150)
  lbl.Size <- new Size(200, 50)
  lbl.BackColor <- Color.LightGray
  lbl.Text <- result
  let font = new Font("Serif", 15.f)
  lbl.Font <- font
  lbl 

let Button num = 
  let but = new Button()
  but.Size <- new Size(50, 50)
  but.Location <- 
    if num = 0 then Point(100, 100)
    else Point(150 + (num - 1) % 3 * 50, ((num - 1) / 3) * 50)
  but.Text <- num.ToString()
  let font = new Font("Serif", 20.f)
  but.Font <- font
  but.MouseClick.Add(fun e -> calc but Label)
  but

let opButton ch num = 
    let but = new Button()
    but.Size <- new Size(50, 50)
    but.Text <- ch
    but.Location <- Point(num % 3 * 50, (num / 3) *50)
    let font = new Font("Serif", 20.f)
    but.Font <- font
    but.MouseClick.Add(fun e -> calc but Label)
    but

let brackButton ch num =
  let but = new Button()
  but.Size <- new Size(50, 50)
  but.Text <- ch
  but.Location <- Point(num * 50, 150)
  let font = new Font("Serif", 20.f)
  but.Font <- font
  but.MouseClick.Add(fun e -> calc but Label)
  but

let array = [|"+";"-"; "*"; "/"; "%" ; "^"; "="; "C"|]


let Form = 
  let form = new Form(Text = "Calculator", Size = new Size(317,237))
  form.Controls.Add(Label)
  for i = 0 to 9 do
    form.Controls.Add(Button i) 
  for i = 0 to 7 do
    form.Controls.Add(opButton array.[i] i)
  form.Controls.Add(brackButton "(" 0)
  form.Controls.Add(brackButton ")" 1)
  form
  

[<EntryPoint>]
let main argv = 
    Form.Visible <- true
    Application.Run()
    0 



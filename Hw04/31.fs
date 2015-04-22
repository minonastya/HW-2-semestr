module test
open NUnit.Framework

type IGraph = 
  interface
    abstract Vertex : int list
    abstract Size : int
    abstract Edge : int -> int -> bool
  end

type GMatrix(vert : int list, edges: bool [,]) =
  class
    interface IGraph with
      member self.Vertex = vert
      member self.Size = vert.Length
      member self.Edge v1 v2= edges.[v1, v2]
  end

type GList(vert : int list, edges: int list []) =
  class
    interface IGraph with
      member self.Vertex = vert
      member self.Size = vert.Length
      member self.Edge v1 v2 = List.exists (fun x -> x = v2) edges.[v1]
  end

let fromVert(G: IGraph, v) =
  if (G.Size > v)&&(v>=0) then
    let array = Array.create G.Size false
    array.[v] <- true
    let rec explore v =
      for i in 0 .. (G.Size - 1) do
        if (G.Edge v i) && (not array.[i]) then
          array.[i] <- true
          explore i
    explore v
    array.[v] <- false
    let mutable res = []
    for i in 0 .. (G.Size - 1) do
      if array.[i] then 
        res <- i::res
    let result = List.rev res
    result
  else []


let toVert(G: IGraph, v) =
  if (G.Size > v)&&(v>=0) then
    let array = Array.create G.Size false
    array.[v] <- true
    let rec explore v =
      for i in 0 .. (G.Size - 1) do
        if (G.Edge i v) && (not array.[i]) then
          array.[i] <- true
          explore i
    explore v
    array.[v] <- false
    let mutable res = []
    for i in 0 .. (G.Size - 1) do
      if array.[i] then 
        res <- i::res
    let result = List.rev res
    result
  else []

type IMarkedGraph<'A> =
  interface
    inherit IGraph 
    abstract labels: 'A array
  end

let v = [0 .. 4]
let arr = Array2D.create 5 5 false
Array2D.set arr 0 1 true
Array2D.set arr 0 4 true               
Array2D.set arr 1 3 true
Array2D.set arr 2 4 true
Array2D.set arr 3 0 true
Array2D.set arr 3 1 true
let list = [|[1; 4]; [3]; [4]; [0; 1]; []|]
let mGraph = new GMatrix(v, arr)
let LGraph = new GList(v, list)

//Тесты на матрицах смежности 
[<Test>]
let ``Список вершин, выходящих из несуществующей в графе вершины`` () =
  let v = [0 .. 9]
  let arr = Array2D.create 10 10 false
  let graph = new GMatrix(v, arr)
  Assert.AreEqual(toVert(graph, 11), [])

[<Test>]
let ``Тест в пустом графе`` () =
  let v = []
  let arr = Array2D.create 0 0 false
  let graph = new GMatrix(v, arr)
  Assert.AreEqual(toVert(graph, 11), [])

[<TestCase (0, Result = [|1; 3; 4|])>]
[<TestCase (1, Result = [|0; 3; 4|])>]
[<TestCase (2, Result = [|4|])>]
[<TestCase (3, Result = [|0; 1; 4|])>]
[<TestCase (4, Result = [||])>]
let ``Списки вершин, в которые можно добраться из данной`` n =
 fromVert (mGraph, n)

//Тесты на списках смежности
[<Test>]
let ``Список вершин, выходящих из несуществующей в графе вершины(list) `` () =
  let v = [0 .. 9]
  let arr = Array.create 10 []
  let graph = new GList(v, arr)
  Assert.AreEqual(toVert(graph, 11), [])

[<Test>]
let ``Тест в пустом графе(list) `` () =
  let v = []
  let arr = Array.create 0 []
  let graph = new GList(v, arr)
  Assert.AreEqual(toVert(graph, 11), [])

[<TestCase (0, Result = [|1; 3|])>]
[<TestCase (1, Result = [|0; 3|])>]
[<TestCase (2, Result = [||])>]
[<TestCase (3, Result = [|0; 1|])>]
[<TestCase (4, Result = [|0; 1; 2; 3|])>]
let ``Списки вершин, из которых можно добраться в данную`` n =
 toVert (LGraph, n)


[<EntryPoint>]
let main argv = 
    let vert = [0..3]
    let array = Array2D.create 4 4 false 
    Array2D.set array 0 1 true
    Array2D.set array 1 2 true                  
    Array2D.set array 2 0 true
    Array2D.set array 2 3 true
    Array2D.set array 3 0 true
    Array2D.set array 3 1 true
    
    let list = [| [1]; [2]; [0; 3]; [0; 1] |]

    printfn "Для графа с вершинами 0, 1, 2, 3: "  
                                       
    printfn "Матрица смежности\n %A\n " array
    printfn "Списки смежности %A\n" list

    let graph1 = new GMatrix(vert, array)
    let gr1 = graph1 :> IGraph
    let graph2 = new GList(vert, list)
    let gr2 = graph2 :> IGraph

    printfn "Существование ребра из вершины 0 в вершину 3: " 
    printfn "%b\n" (gr1.Edge 0 3)

    printfn "Существование ребра из вершины 1 в вершину 2: " 
    printfn "%b\n" (gr2.Edge 1 2)
            
    printfn "Вершины, доступные из 0: "
    printfn "%A\n" (fromVert(gr1, 0))

    printfn "Вершины, из которых можно попасть в 2: "
    printfn "%A\n" (toVert(gr2, 2))
    0 
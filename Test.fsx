open System


printfn "Ready"
printf "%d" (int (Console.ReadLine()))

let rec makeList (s:string) =
    Array.map int (s.Replace(" ", "").Split [|','|])

makeList "5,0, 2 , 1"

let mapArrays =
   Map.empty. (* Creating an empty Map *)
      Add("Zara Ali", [|1;2;3|]);;

let mapInts =
   Map.empty. (* Creating an empty Map *)
      Add("Zara Ali", 5);;

Array.set (Map.find "Zara Ali" mapArrays) 2 5
mapInts.Add("test", 3)
mapArrays
open System


printfn "Ready"
printf "%d" (int (Console.ReadLine()))

let rec makeList (s:string) =
    Array.map int (s.Replace(" ", "").Split [|','|])

makeList "5,0, 2 , 1"
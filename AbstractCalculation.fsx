module AbstractCalculation

let rec powS s1 s2 = 
    match(s1,s2) with
    | (Minus, Minus) 
    | (Minus, Plus) -> Set.ofList [Minus; Plus]
    | (Zero, Minus)
    | (Zero, Plus) -> Set.singleton Zero
    | _ -> Set.singleton Plus

let uplusS s = s;

let rec uminusS s = 
    match s with
    | Minus -> Plus
    | Plus -> Minus
    | _ -> Zero
and uminusSetS set =
    Set.map uminusS set
let rec addS s1 s2 =
    match(s1,s2) with
    |(Minus,Minus) 
    |(Zero,Minus) 
    |(Minus, Zero) -> Set.singleton Minus
    |(Zero, Zero)  -> Set.singleton Zero
    |(Zero,Plus)
    |(Plus,Zero)
    |(Plus,Plus) -> Set.singleton Plus
    | _ -> Set.ofList [Minus; Plus; Zero];

let rec subS s1 s2 =
    match(s1,s2) with
    |(Zero, Plus)
    |(Minus, Zero)
    |(Minus, Plus) -> Set.singleton Minus
    |(Zero, Zero) -> Set.singleton Zero
    |(Zero, Minus)
    |(Plus, Zero)
    |(Plus, Minus) -> Set.singleton Plus
    | _ -> Set.ofList [Minus; Plus; Zero];

let rec timesS s1 s2 =
    match(s1,s2) with
    | (Minus, Minus)
    | (Plus, Plus) -> Set.singleton Plus
    | (Minus, Plus)
    | (Plus, Minus) -> Set.singleton Minus
    | _ -> Set.singleton Zero

let rec divideS s1 s2 =
    match(s1,s2) with
    | (Minus, Minus)
    | (Plus, Plus) -> Set.ofList [Plus; Zero]
    | (Minus, Plus)
    | (Plus, Minus) -> Set.ofList [Minus; Zero]
    | (_, Zero) -> Set.empty
    | _ -> Set.singleton Zero

let rec greaterS s1 s2 = 
    match (s1, s2) with
    | (Minus, Minus) 
    | (Plus, Plus) -> Set.ofList [TrueBool; FalseBool]
    | (Zero, Minus)
    | (Plus, Minus)
    | (Plus, Zero) -> Set.singleton TrueBool
    | _ -> Set.singleton FalseBool

let rec greaterEqualS s1 s2 = 
    match (s1, s2) with
    | (Minus, Plus)
    | (Zero, Plus)
    | (Minus, Zero) -> Set.singleton FalseBool
    | (Zero, Minus)
    | (Zero, Zero)
    | (Plus, Minus)
    | (Plus, Zero) -> Set.singleton TrueBool
    | (Minus, Minus)
    | (Plus, Plus) -> Set.ofList [TrueBool; FalseBool];

let rec lessS s1 s2 = 
    match (s1, s2) with
    | (Minus, Minus)
    | (Plus, Plus) -> Set.ofList [TrueBool; FalseBool]
    | (Zero, Plus)
    | (Minus, Zero) 
    | (Minus, Plus) -> Set.singleton TrueBool
    | _ -> Set.singleton FalseBool

let rec lessEqualsS s1 s2 = 
    match (s1, s2) with
    | (Minus, Minus)
    | (Plus, Plus) -> Set.ofList [TrueBool; FalseBool]
    | (Zero, Minus)
    | (Plus, Minus)
    | (Plus, Zero) -> Set.singleton FalseBool
    | _ -> Set.singleton TrueBool

let rec equalS s1 s2 = 
    match (s1, s2) with
    | (Minus, Minus)
    | (Plus, Plus)   -> Set.ofList [TrueBool; FalseBool]
    | (Zero, Zero)   -> Set.singleton TrueBool
    | _ -> Set.singleton FalseBool

let rec notEqualS s1 s2 = 
    match (s1, s2) with
    | (Minus, Minus)
    | (Plus, Plus)   -> Set.ofList [TrueBool; FalseBool]
    | (Zero, Zero) -> Set.singleton FalseBool
    | _ -> Set.singleton TrueBool

let rec orS s1 s2 = 
    match (s1, s2) with
    | (FalseBool, FalseBool) -> Set.singleton FalseBool
    | _ -> Set.singleton TrueBool

let rec andS s1 s2 = 
    match (s1, s2) with
    | (TrueBool, TrueBool) -> Set.singleton TrueBool
    | _ -> Set.singleton FalseBool;

let rec nutS s = 
    match s with
    | TrueBool -> FalseBool
    | _ -> TrueBool
Run GCL.fsx with fsharpi to run the program  
Then enter a flag and a guarded command  
Example: -ND if a < 4 -> a:=a+1 fi  
The flags are:  
-ND -> non deterministic graph  
-D -> deterministic graph  
-P -> print syntax tree  
-SW -> Stepwise execution, it asks you to enter the start values,
    for variable its just a number, for array seperate your numbers  with a ","  
    ex.  
    Enter initial value for x  
    5  
    Enter initial value for A:  
    1,2,3,4  
-SA -> Sign Analysis  
        start by declaring number of init memories  
        then for vars enter + - or 0
        and for array seperate them by a ","  
        Example:   
        Enter initial signs for A:  
        +,-,0  
        Enter initial sign for a  
        +  
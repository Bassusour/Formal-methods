# Formal methods Group 45
Run GCL.fsx with fsharpi to run the program  
Then enter a flag and a guarded command  
Example: -ND if a < 4 -> a:=a+1 fi  
The flags are:  
* -ND -> non deterministic graph  
* -D -> deterministic graph  
* -P -> print syntax tree  
* -SW -> Stepwise execution, it asks you to enter the start values,
    * for variable its just a number, for array seperate your numbers  with a ","  
    * Example:  
        Enter initial value for x  
        5  
        Enter initial value for A:  
        1,2,3,4  
* -SA -> Sign Analysis  
    * start by declaring number of init memories  
    * then for vars enter + - or 0
    * and for array seperate them by a ","  
    * Example:   
        Enter initial signs for A:  
        +,-,0  
        Enter initial sign for a  
        +  
* -SEC -> Security  
    * start by giving a security lattice  
    * then enter initial variable security  
    * Example:  
         Enter security lattice
         trusted < dubious, public < private
         Enter initial variable security
         x = trusted, y = private
* -MC -> Model Checking  
    * Start by giving init values
    * Example:  
         -MC if a<0 -> A[0]:=-1 [] a>0 -> A[1] := 1 fi
         Enter initial values
         a=0, A=[1;2]
         Stuck States:
         qs: A0 = 1, A1 = 2, a = 0, a
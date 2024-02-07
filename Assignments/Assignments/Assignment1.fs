module Assignment1

    open System
    
    let sqr x = x * x 


    let pow x n = System.Math.Pow(x, n)
    

    let rec sum = function
        | 0 -> 0
        | n -> n + sum (n-1)


    let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | n -> fib (n-1) + fib (n-2)    


    let dup s:string = (s + s)


    let rec dupn (s:string) (n:int) = 
        match n with
        | 0 -> ""
        | n -> s + dupn s (n-1)


    let rec bin (n:int, k:int) = 
        if k=0 || k=n then 1
        else bin (n-1, k-1) + bin (n-1, k) 
        
           
    let timediff (t1: int * int) (t2: int * int) = 
        let (x1, y1) = t1
        let (x2, y2) = t2

        let t1 = x1*60 + y1
        let t2 = x2*60 + y2
        
        t2-t1


    let minutes (x1:int, y1:int) =
        timediff(0, 0) (x1, y1) 


    let curry _ = failwith "not implemented"
    let uncurry _ = failwith "not implemented"

    let empty _ = failwith "not implemented"

    let add _ = failwith "not implemented"

    let singleLetterScore _ = failwith "not implemented"
    let doubleLetterScore _ = failwith "not implemented"
    let trippleLetterScore _ = failwith "not implemented"

    let hello _ = failwith "not implemented"
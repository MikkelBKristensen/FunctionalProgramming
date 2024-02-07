﻿module Assignment2

    open Microsoft.FSharp.Collections

    let rec downto1 =
        fun n ->
        if n > 0 then [n] @ downto1 (n-1) 
        else []
    
    let rec downto2 =
        function
        | n when n>0 -> [n] @ downto1 (n-1)
        | _ -> []

    let rec removeOddIdx (xs: 'a list) =
        match xs with
        | head::head2::tail -> head :: removeOddIdx tail
        | [_] -> xs
        | _ -> []
        
        
        
    let rec combinePair (xs: 'a list) =
        match xs with
        | head::head2::tail -> (head, head2) :: combinePair tail
        | _ -> []
        

    type complex = unit // Fill in your type here
    let mkComplex _ = failwith "not implemented"
    let complexToPair _ = failwith "not implemented"
    let (|+|) _ = failwith "not implemented"
    let (|*|) _ = failwith "not implemented"
    let (|-|) _ = failwith "not implemented"
    let (|/|) _ = failwith "not implemented"

    let explode1 (s:string) =
        if s.Length > 1 then
            Seq.toList(s)
        else []
        
    let rec explode2 (s:string) =
        match s with
        | "" -> []
        | s -> s[0] :: explode2 s[1..]

    let implode (cs: list<char>) = s:string + 
        
        
    let implodeRev _ = failwith "not implemented"

    let toUpper _ = failwith "not implemented"

    let ack _ = failwith "not implemented"



    let downto3 _ = failwith "not implemented"

    let fac _ = failwith "not implemented"
    let range _ = failwith "not implemented"

    type word = (char * int) list


    let hello = [] // Fill in your answer here

    type squareFun = (char * int) list -> int -> int -> int

    let singleLetterScore _ = failwith "not implemented"
    let doubleLetterScore _ = failwith "not implemented"
    let tripleLetterScore _ = failwith "not implemented"

    let doubleWordScore _ = failwith "not implemented"
    let tripleWordScore _ = failwith "not implemented"

    type square = (int * squareFun) list


    let oddConsonants _ = failwith "not implemented"
    
    

    let SLS : square = [(0, singleLetterScore)]
    let DLS : square = [(0, doubleLetterScore)]
    let TLS : square = [(0, tripleLetterScore)]

    let DWS : square = SLS @ [(1, doubleWordScore)]
    let TWS : square = SLS @ [(1, tripleWordScore)]

    let calculatePoints _ = failwith "not implemented"

module Assignment2

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
        

    type complex = { Real : float; Imaginary : float } 
    let mkComplex float1 float2 = 
        let number = { Real = float1; Imaginary = float2 }
        number
    let complexToPair (complexNumber: complex) = 
        (complexNumber.Real, complexNumber.Imaginary)

    let (|+|) (number1: complex) (number2: complex) = 
        let complexNumber = { Real = number1.Real + number2.Real; Imaginary = number1.Imaginary + number2.Imaginary }  
        complexNumber

    let (|*|) (number1: complex) (number2: complex) = 
        let complexNumber = { Real = (number1.Real * number2.Real) - (number1.Imaginary * number2.Imaginary); Imaginary = (number1.Imaginary * number2.Real) + (number1.Real * number2.Imaginary) }
        complexNumber

    let (|-|) (number1: complex) (number2: complex) = 
        let complexNumber =  { Real = (number1.Real - number2.Real); Imaginary = (number1.Imaginary - number2.Imaginary) }
        complexNumber

    let (|/|) (number1: complex) (number2: complex) = 
        let divisor = number2.Real * number2.Real + number2.Imaginary * number2.Imaginary
        let realPart = (number1.Real * number2.Real + number1.Imaginary * number2.Imaginary) / divisor
        let imaginaryPart = (number1.Imaginary * number2.Real - number1.Real * number2.Imaginary) / divisor
        let complexNumber = { Real = realPart; Imaginary = imaginaryPart }
        complexNumber

    let explode1 (s:string) =
        if s.Length > 1 then
            Seq.toList(s)
        else []
        
    let rec explode2 (s:string) =
        match s with
        | "" -> []
        | s -> s[0] :: explode2 s[1..]

    let implode (cs: list<char>) : string =            
        let f = fun char acc -> string char + acc    // defines function f that takes two arguments, char and acc. The function then converts char to a string and concatenates with acc
        List.foldBack f cs ""                        // Call function f on all values in cs from back with where char is the cs list and acc is the empty string
        
    let implodeRev (cs: list<char>) : string =       // Does the same as above, just from head to tail. Because of this, the order of arguments is reversed too.
        let f = fun acc char -> string char + acc
        List.fold f "" cs
        
    let toUpper (input: string) : string =
        let y = input.ToUpper() // Converts input to uppercase
        y                       // Returns the uppercase string

    let rec ack (m, n) = 
        match m with
        | 0 when m=0 -> n + 1
        | m when m > 0 && n = 0 -> ack (m-1, 1)
        | m when m > 0 && n > 0 -> ack (m-1, ack (m, n-1))
        
    let time f =
      let start = System.DateTime.Now
      let res = f ()
      let finish = System.DateTime.Now
      (res, finish - start)

    time (fun () -> ack (3, 11))

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

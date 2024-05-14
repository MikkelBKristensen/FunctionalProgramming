module Assignment5

(* Exercise 5.1 *)

let sum (m: int) (n: int) : int =
    let rec sumIterative (m: int) (n: int) (acc: int) =
        if n = 0 then
            acc + m
        else
            sumIterative m (n - 1) (m + n + acc)
    sumIterative m n 0

(* Exercise 5.2 *)

let length (lst: 'a list) =
    let rec listLength (lst: 'a list) (acc: int) =
        match lst with
        | [] -> acc
        | _ :: xs -> listLength xs (acc + 1)
    listLength lst 0

(* Exercise 5.3 *)

let foldBack (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) = 
    let rec foldBackCont lst acc cont =
        match lst with
        | [] -> cont acc
        | x::xs -> foldBackCont xs acc (fun acc' -> cont (f x acc'))
    foldBackCont lst acc (fun x -> x)


(* Exercise 5.4 *)

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x
    
let factC x =
    let rec factCont x acc cont =
        match x with
        | 0 -> cont acc
        | _ -> factCont (x-1) acc (fun acc' -> cont (x * acc'))
    factCont x 1 (fun x -> x)

let timer f x =
    let start = System.DateTime.Now
    let res = f x
    let stop = System.DateTime.Now
    (stop - start).TotalSeconds

(* Compare the running time between factA and factC. Which solution is faster and why? 
   After using the timer, I can conclude that the running time of factA and factC is extremely close,
   as both functions perform a similar number of multiplicative operations.
   However, using factC has the advantage of explicitly managing the control flow using continuations,
   which can help avoid stack overflow issues in scenarios where tail call optimization might not be
   consistently applied by the compiler.
*)

(* Exercise 5.5 *)

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA _ = failwith "not implemented"

let fibC _ = failwith "not implemented"


(* TODO: *)
(* Compare the running time of fibW, fibA and fibC. Which solution is faster and why? 
   <Your answer goes here>

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow. 
   Analyse the problem and describe exactly why this happens. 
   Why is this not an iterative function?
   
   To make a compelling argument (and to prepare for the exam) you must make a step-by-step evalution of a call to
   bigListK. Test correctness of your evaluations by ensuring that they all evaluate to the same value. For example:
   
   (5 + 4) * 3 -->
   9 * 3 -->
   27
   
   If you input any of these lines into FSharp Interactive it will produce the same result. Do the same here and point
   to where you can see that this function is not tail recursive.

   <Your answer goes here>
*)

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | CharToInt of cExp


and cExp =
   | C  of char  (* Character value *)
   | CV of aExp  (* Character lookup at word index *)
   | ToUpper of cExp
   | ToLower of cExp
   | IntToChar of aExp


let arithEvalSimple _ = failwith "not implemented"

let charEvalSimple _ = failwith "not implemented"

let arithEval _ = failwith "not implemented"
let charEval _ = failwith "not implemented"
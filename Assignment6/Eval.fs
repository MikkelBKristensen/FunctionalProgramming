﻿module Eval

    open StateMonad
    open Types

    (* Code for testing *)

    let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]  
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x + y)
        
    let sub (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x - y)
        
    let mul (a : SM<int>) (b : SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x * y)
     
    let div a b =
        a >>= fun x ->
        b >>= fun y ->
            if (y <> 0) then ret (x / y)
            else fail DivisionByZero
    
    let modulo a b =
        a >>= fun x ->
        b >>= fun y ->
            if (y <> 0) then ret (x % y)
            else fail DivisionByZero
    
    let aEquality (a : SM<int>) (b : SM<int>) : SM<bool> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x = y)
        
    let aLessThan (a : SM<int>) (b : SM<int>) : SM<bool> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x < y)
    
    let aConj (a : SM<bool>) (b : SM<bool>) : SM<bool> =
        a >>= fun x ->
        b >>= fun y ->
        ret (x && y)
        
    let aNot (a : SM<bool>) : SM<bool> =
        a >>= fun x ->
        ret (not x)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval (a : aExp) : SM<int> =
        match a with
        | N n -> ret n 
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= pointValue
        | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> sub (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> mul (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> modulo (arithEval a1) (arithEval a2)
        | CharToInt c -> charEval c >>= fun x -> ret (int x)
    and charEval (c : cExp) : SM<char> =
        match c with
        | C c -> ret c                              (* Character value *)
        | CV cv -> arithEval cv >>= characterValue  (* Character lookup at word index *)
        | ToUpper c -> charEval c >>= fun x -> ret (System.Char.ToUpper x)
        | ToLower c -> charEval c >>= fun x -> ret (System.Char.ToLower x)
        | IntToChar c -> arithEval c >>= fun x -> ret (char x)
           
    and  boolEval (b : bExp) : SM<bool> =
        match b with
        | TT -> ret true                   (* true *)
        | FF -> ret false                  (* false *)

        | AEq (b1, b2) -> aEquality (arithEval b1) (arithEval b2)    (* numeric equality *)
        | ALt (b1, b2) -> aLessThan (arithEval b1) (arithEval b2)    (* numeric less than *)

        | Not b -> boolEval b >>= fun x -> ret (not x)                (* boolean not *)
        | Conj (b1, b2) -> aConj (boolEval b1) (boolEval b2)                       (* boolean conjunction *)

        | IsVowel c -> charEval c >>= fun x -> ret("aeiouyæøå".Contains(System.Char.ToLower(x)))    (* check for vowel *)
        | IsLetter c -> charEval c >>= fun x -> ret(System.Char.IsLetter x)                         (* check for letter *)
        | IsDigit c -> charEval c >>= fun x -> ret(System.Char.IsDigit x)                           (* check for digit *)


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"
    
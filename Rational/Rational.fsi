module Ration.Rational
[<Sealed>]

type Rat = 
    static member ( + ) :
        Rat * Rat -> Rat
    static member ( - ) :
        Rat * Rat -> Rat
    static member ( * ) :
        Rat * Rat -> Rat
    static member ( / ) :
        Rat * Rat -> Rat

val mkRat : int -> int -> Rat

val add : Rat -> Rat -> Rat

val sub : Rat -> Rat -> Rat

val mul : Rat -> Rat -> Rat

val div : Rat -> Rat -> Rat
module Rational

type Rat = R of int * int

let mkRat a b = R (a,b)

let add (R (a,b)) (R (c,d))= mkRat (a * d + b * d) (b*d)

let sub (R (a,b)) (R (c,d))= mkRat (a * d + b * d) (b*d)

let mul (R (a,b)) (R (c,d))= mkRat (a * d + b * d) (b*d)

let div (R (a,b)) (R (c,d))= mkRat (a * d) (b * c)
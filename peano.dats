(* A template for single-file ATS programs *)

(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

(* ****** ****** *)

datatype
peano = 
  zero of () | succ of (peano)

fun peano_to_int (ns: peano): int = 
  case+ ns of 
  | zero()  => 0 
  | succ(n) => 
      1 + peano_to_int(n)

fun peano_add (n: peano, m: peano): peano = 
  case (n, m) of 
  | (zero(), x)  => x
  | (x, zero())  => x
  | (succ(x), y) => peano_add(x, succ(y))

fun peano_mult (n: peano, m: peano): peano = 
  case (n, m) of 
  | (zero(), x)       => zero
  | (x, zero())       => zero
  | (succ(zero()), x) => x 
  | (x, succ(zero())) => x 
  | (succ(x), y)      => 
      peano_add (peano_mult(x,y), y)

(* Data *)

val one   = succ(zero)
val two   = succ(one)
val three = succ(two)

(* *** *)

val _ = println!("2 * 3 = ", peano_to_int(
  peano_mult (two, three) ))

val _ = println!("2 + 3 = ", peano_to_int(
  peano_add (two, three) ))

(* ****** ****** *)

implement main0 () = () // a dummy implementation for [main]

////

To compile:

```
  myatscc peano.dats

```

To run: 

```
  ./peano_dats 

```

Explanations: 


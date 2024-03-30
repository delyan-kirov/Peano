(*
** Simple program for creating the natural numbers in ATS
*)

(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

(* ****** ****** *)

datatype
peano = 
  zero of () | succ of (peano)

(* Constants *)

val one   = succ(zero)
val two   = succ(one)
val three = succ(two)

(* Function *)

fun peano_to_int (ns: peano): int = 
  case+ ns of 
  | zero()  => 0 
  | succ(n) => 1 + peano_to_int(n)

fun int_to_peano {n: int | n >= 0} (m: int n) =
  if m = 0 then zero
  else succ(int_to_peano(m - 1))

fun peano_add (n: peano, m: peano): peano = 
  case (n, m) of 
  | (zero(), x)  => x
  | (x, zero())  => x
  | (succ(x), y) => succ(peano_add(x, y))

fun peano_mult (n: peano, m: peano): peano = 
  case (n, m) of 
  | (zero(), x)       => zero
  | (x, zero())       => zero
  | (succ(x), y)      => peano_add (peano_mult(x,y), y)

(* ****** ****** *)

implement main0 () = {
  val _ = println!("2 * 3 = ", peano_to_int(
    peano_mult(two, three)))
  
  val _ = println!("3 + 3 = ", peano_to_int(
    peano_add(three, three)))
  
  val _ = println!("69 + 666 = ", peano_to_int(
    peano_add(
      int_to_peano(69),
      int_to_peano(666)
    )
  ))
  
  val _ = println!("69 * 3 = ", peano_to_int(
    peano_mult(
      int_to_peano(69),
      int_to_peano(3)
    )
  ))
}

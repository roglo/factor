(* $Id: nrandom.ml,v 1.1 2003/01/10 13:24:11 ddr Exp $ *)

open Num;
open Printf;

value zero = Int 0;
value one = Int 1;
value two = Int 2;

value rand_aux =
  loop zero where rec loop res x =
    if eq_num x zero then res
    else
      let res = mult_num res two in
      loop (if Random.bool () then add_num res one else res) (quo_num x two)
;
value rec rand_num a =
  if le_num a zero then invalid_arg "rand_num"
  else
    let res = rand_aux a in
    if ge_num res a then rand_num a else res
;

value main () =
  do {
    Random.self_init ();
    let r = rand_num (power_num (Int 10) (num_of_string Sys.argv.(1))) in
    printf "%s\n" (string_of_num r);
    flush stdout
  }
;

main ();

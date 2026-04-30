(* $Id: truncable.ml,v 1.2 2003/02/25 17:49:03 ddr Exp $ *)

(* recherche du plus long nombre premier tronquable, c'est-à-dire si
   on lui enlève un nombre quelconque de ses premiers chiffres, c'est
   toujours un nombre premier *)

open Printf;
open Num;

value zero = Int 0;
value one = Int 1;
value two = Int 2;
value three = Int 3;
value four = Int 4;
value five = Int 5;
value ten = Int 10;

value quomod_num a b =
  let (q, r) = Big_int.quomod_big_int (big_int_of_num a) (big_int_of_num b) in
  (num_of_big_int q, num_of_big_int r)
;

value power_num_mod a b p =
  loop one a b where rec loop res a b =
    if eq_num b zero then res
    else
      let (q, r) = quomod_num b two in
      let new_a = mod_num (square_num a) p in
      if eq_num r zero then loop res new_a q
      else loop (mod_num (mult_num res a) p) new_a q
;

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

value is_probably_prime p =
  let p_minus_one = pred_num p in
  let (s, d) =
    loop 0 p_minus_one where rec loop s d =
      let (q, r) = quomod_num d two in
      if eq_num r zero then loop (succ s) q
      else (s, d)
  in
(*
let _ = do { printf "p-1 = d * 2^%d\n" s; flush stdout; } in
*)
  (* test Fermat with Miller-Rabin criteron *)
  let test_fermat a =
(*
let _ = do { printf "test Fermat avec %s" (string_of_num a); flush stdout; } in
*)
(**)
    let v = power_num_mod a d p in
    if eq_num v one then
(*
let _ = do { printf " -> 1\n"; flush stdout } in
*)
      True
    else
      loop v s where rec loop v s =
        if eq_num v p_minus_one then
(*
let _ = do { printf " -> -1 ...\n"; flush stdout } in
*)
          True
        else
          let new_v = mod_num (square_num v) p in
          if eq_num new_v one then
(*
let _ = do { printf " -> mauvaise racine de 1: %s\n" (string_of_num v); flush stdout } in
*)
            False
          else if s = 1 then
(*
let _ = do { printf " -> %s\n" (string_of_num new_v); flush stdout } in
*)
            False
          else loop new_v (s - 1)
  in
  let rec loop_test cnt =
    if cnt <= 0 then True
    else
      let a = add_num two (rand_num (sub_num p three)) in
      test_fermat a && loop_test (cnt - 1)
  in
  loop_test 4
;

value divisible_by_small m n =
  loop m two zero where rec loop cnt d dd =
    if le_num n d then eq_num n two
    else if cnt < 0 then False
    else if eq_num zero (mod_num n d) then True
    else if d = two then loop (cnt - 1) three zero
    else if d = three then loop (cnt - 1) five two
    else loop (cnt - 1) (add_num d dd) (if eq_num dd two then four else two)
;

value rec is_prime n =
  if eq_num n two || eq_num n three then True
  else if divisible_by_small 200 n then False
  else
    do {
(*
      printf "\n*** %s\n" (string_of_num n);
      flush stdout;
*)
      is_probably_prime n || is_prime (add_num n two)
    }
;

value reverse n =
  loop zero n where rec loop res n =
    if eq_num n zero then res
    else
      let (q, r) = quomod_num n ten in
      loop (add_num (mult_num res ten) r) q
;

value rec truncable mlen len n =
  let rev_n = reverse n in
  if is_prime rev_n then
let _ = do { let s = string_of_num rev_n in printf "%s%s\027[K\r" (String.make (mlen - String.length s) ' ') s; flush stdout } in
    do {
      let len = len + 1 in
      let nn = succ_num (mult_num n (Int 10)) in
      if len > mlen then
        do {
          printf "%s\n" (string_of_num rev_n);
          flush stdout;
        }
      else ();
      truncable (max mlen len) len nn
    }
  else
(**)
    let (n, len) =
      let n = succ_num n in
      let (q, r) = quomod_num n ten in
      if eq_num r zero then
        loop q (pred len) where rec loop n len =
          if eq_num n one then raise Exit
          else
            let (q, r) = quomod_num n ten in
            if eq_num r zero then loop q (pred len)
            else (n, len)
      else (n, len)
    in
    truncable mlen len n
(*
    let n =
      loop (succ_num n) where rec loop n =
        if eq_num n one then raise Exit
        else
          let (q, r) = quomod_num n ten in
          if eq_num r zero then loop q
          else n
    in
    truncable mlen (String.length (string_of_num n)) n
*)
;

try truncable 1 1 (Int 2) with [ Exit -> () ];

(* $Id: rho_pollard.ml,v 1.3 2009/12/08 02:09:57 deraugla Exp $ *)

open Printf;
open Bnum_def;

value short_string_of_big num n =
  let s = num.to_string n in
  let len = String.length s in
  if len > 30 then
    String.sub s 0 10 ^ "..." ^ String.sub s (len - 10) 10 ^ " (" ^
    string_of_int (String.length s) ^ " digits)"
  else s
;

value max_it = ref max_int;

(* Pollard rho with Brent algo *)
(* compares u_i and u_k where k = power of 2 less to i *)
value factor_brent num verbose n =
  let pp x = num.add_int (num.mul x x) 1 in
  loop 0 1 num.zero num.two 0 num.one
  where rec loop i r x y xmycnt xmy = do {
    if verbose then do {
      eprintf "r=%d i=%d %s\027[K\r" r i (short_string_of_big num y);
      flush stderr
    }
    else ();
    (* to accelerate, computing gcd only once over 30 *)
    if xmycnt < 30 || num.eq_int (num.gcd n (num.abs xmy)) 1 then
      let ny = num.mod_ (pp y) n in
      let (xmycnt, xmy) =
        if xmycnt = 30 then (0, num.one)
        else (xmycnt + 1, num.mul xmy (num.sub x y))
      in
      if i > max_it.val then None
      else if r > i then loop (i + r) 1 y ny xmycnt xmy
      else loop i (r + 1) x ny xmycnt xmy
    else do {
      let g = num.gcd n (num.abs xmy) in
      let d = num.div n g in
      if verbose then do {
        eprintf "r=%d %s\027[K\r\n" r (short_string_of_big num y);
        flush stderr
      }
      else ();
      if num.eq_int g 1 || num.eq_int d 1 then None
      else Some (g, d)
    }
  }
;

(* Pollard rho with Floyd algo *)
(* compares u_i and u_2i *)
value factor_floyd num verbose n =
  let pp x = num.add_int (num.mul x x) 1 in
  loop 1 num.two num.two 0 num.one where rec loop i x y xmycnt xmy = do {
    let x = num.mod_ (pp x) n in
    let y = num.mod_ (pp (pp y)) n in
    if verbose then do {
      eprintf "i=%d %s\027[K\r" i (short_string_of_big num x);
      flush stderr
    }
    else ();
    if xmycnt < 30 || num.eq_int (num.gcd n (num.abs xmy)) 1 then
      let (xmycnt, xmy) =
        if xmycnt = 30 then (0, num.one)
        else (xmycnt + 1, num.mul xmy (num.sub x y))
      in
      if i > max_it.val then None
      else loop (i + 1) x y xmycnt xmy
    else do {
      let g = num.gcd n (num.abs xmy) in
      let d = num.div n g in
      if verbose then do {
        eprintf "i=%d %s\027[K\r\n" i (short_string_of_big num x);
        flush stderr
      }
      else ();
      if num.eq_int g 1 || num.eq_int d 1 then None
      else Some (g, d)
    }
  }
;

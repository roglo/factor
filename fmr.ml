(* $Id: fmr.ml,v 1.6 2009/12/08 11:09:09 deraugla Exp $ *)

open Bnum_def;
open Printf;

type probably_prime 'a =
  [ PrOne
  | PrMinusOne
  | PrBadSqrtOne of 'a
  | PrNotOne of 'a ]
;

type t 'a =
  [ Prime
  | Factor of list 'a and list 'a
  | Composite ]
;

type info 'a = ('a * int * 'a);

value init_fermat_test num p =
  let p_minus_one = num.sub_int p 1 in
  let (s, d) =
    loop 0 p_minus_one where rec loop s d =
      let (q, r) = num.quomod_int d 2 in
      if r = 0 then loop (succ s) q else (s, d)
  in
  (p_minus_one, s, d)
;

value fermat_miller_rabin_test num p (p_minus_one, s, d) a =
  (* test Fermat with Miller-Rabin criteron *)
  let v = num.power_mod a d p in
  if num.eq_int v 1 then PrOne
  else
    let rec loop v s =
      if num.eq v p_minus_one then PrMinusOne
      else
(*
        let new_v = num.mod_ (num.mul v v) p in
*)
        let new_v = num.power_mod_int v 2 p in
(**)
        if num.eq_int new_v 1 then PrBadSqrtOne v
        else if s = 1 then PrNotOne new_v
        else loop new_v (s - 1)
    in
    loop v s
;

value rec test num p =
  if num.mod_int p 2 = 0 then Composite
  else
    let info = init_fermat_test num p in
    loop_test 12 False where rec loop_test cnt surely_composite =
      if cnt <= 0 then if surely_composite then Composite else Prime
      else
        let a = num.add_int (Bnum.random num (num.sub_int p 3)) 2 in
        match fermat_miller_rabin_test num p info a with
        [ PrOne -> loop_test (cnt - 1) surely_composite
        | PrMinusOne -> loop_test (cnt - 1) surely_composite
        | PrBadSqrtOne v ->
            let p1 = num.gcd p (num.sub_int v 1) in
            let p2 = num.gcd p (num.add_int v 1) in
            let (pl1, pl2) =
              List.fold_left
                (fun (pl1, pl2) p ->
                   match test num p with
                   [ Prime -> ([p :: pl1], pl2)
                   | Factor spl1 spl2 -> (spl1 @ pl1, spl2 @ pl2)
                   | Composite -> (pl1, [p :: pl2]) ])
                ([], []) [p1; p2]
            in
            Factor (List.sort num.compare pl1) (List.sort num.compare pl2)
        | PrNotOne v -> loop_test (cnt - 1) True ]
;

value is_probably_prime num verbose p =
  let ((_, s, d) as info) = init_fermat_test num p in
  let _ =
    if verbose then do { eprintf "p-1 = d * 2^%d\n" s; flush stderr } else ()
  in
  loop_test 8 where rec loop_test cnt =
    if cnt <= 0 then True
    else
      let a =
        if cnt = 8 then num.of_int 2
        else num.add_int (Bnum.random num (num.sub_int p 3)) 2
      in
      let _ =
        if verbose then do {
          eprintf "test Fermat with %s" (Bnum.to_short_string num a);
          flush stderr
        }
        else ()
      in
      match fermat_miller_rabin_test num p info a with
      [ PrOne ->
          let _ =
            if verbose then do { eprintf " -> 1\n"; flush stderr } else ()
          in
          loop_test (cnt - 1)
      | PrMinusOne ->
          let _ =
            if verbose then do { eprintf " -> -1 ...\n"; flush stderr } else ()
          in
          loop_test (cnt - 1)
      | PrBadSqrtOne v ->
          let _ =
            if verbose then do {
              eprintf " -> mauvaise racine de 1: %s\n" (Bnum.to_string num v);
              flush stderr
            }
            else ()
          in
          False
      | PrNotOne v ->
          let _ =
            if verbose then do {
              eprintf " -> %s\n" (Bnum.to_short_string num v);
              flush stderr
            }
            else ()
          in
          False ]
;

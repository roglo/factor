(* $Id: fib2.ml,v 1.3 2007/03/12 10:34:42 deraugla Exp $ *)
(* another way to compute fibonacci modulo *)

open Big_int;
open Printf;

value one_big_int = unit_big_int;
value two_big_int = big_int_of_int 2;

module MapInt =
  Map.Make (struct type t = int; value compare = compare; end)
;

value record () =
  let rec_map = ref MapInt.empty in
  fun f x ->
    match
      try Some (MapInt.find x rec_map.val) with [ Not_found -> None ]
    with
    [ Some r -> r
    | None -> do {
        let r = f x in
        rec_map.val := MapInt.add x r rec_map.val;
        r
      } ]
;

value u_mod i m =
  let record_u = record () in
  let record_v = record () in
  let rec u i = record_u compute_u i
  and compute_u i =
    if i <= 1 then big_int_of_int i
    else
      let n = i / 2 in
      let r =
        if i mod 2 = 0 then mult_big_int (u n) (v n)
        else if n mod 2 = 1 then
          add_big_int (mult_big_int (u (n + 1)) (v n)) one_big_int
        else
          sub_big_int (mult_big_int (u (n + 1)) (v n)) one_big_int
      in
      mod_big_int r m
  and v i = record_v compute_v i
  and compute_v i =
    if i = 0 then two_big_int
    else if i = 1 then one_big_int
    else
      let n = i / 2 in
      let r =
        if i mod 2 = 0 then
          add_int_big_int (2 * (if n mod 2 = 1 then 1 else -1))
            (square_big_int (v n))
        else
          add_int_big_int (if n mod 2 = 1 then 1 else -1)
            (mult_big_int (v (n + 1)) (v n))
      in
      mod_big_int r m
  in
  u i
;

value main () = do {
  Printf.printf "%s\n"
    (string_of_big_int
       (u_mod (int_of_string Sys.argv.(1))
       (big_int_of_string Sys.argv.(2))));
  flush stdout;
};

main ();

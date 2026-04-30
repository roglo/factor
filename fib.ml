(* $Id: fib.ml,v 1.3 2007/03/11 14:03:35 deraugla Exp $ *)
(* computing fibonacci modulo *)

open Big_int;

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

value fib_mod n m =
  let record = record () in
  let rec fib n = record fib0 n
  and fib0 n =
    if n <= 1 then big_int_of_int n
    else
      let q = n / 2 in
      let r =
        if n mod 2 = 0 then
          add_big_int (square_big_int (fib q))
            (mult_int_big_int 2 (mult_big_int (fib q) (fib (q - 1))))
        else 
          add_big_int (square_big_int (record fib q))
            (square_big_int (record fib (q + 1)))
      in
      mod_big_int r m
  in
  fib n
;

Printf.printf "%s\n"
  (string_of_big_int
     (fib_mod (int_of_string Sys.argv.(1))
        (big_int_of_string Sys.argv.(2))));
flush stdout;

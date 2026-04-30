(* $Id: what.ml,v 1.3 2009/12/19 13:13:24 deraugla Exp $ *)

open Bnum_def;
open Printf;

value e3 = Stringexp.e3;

value what (num : Bnum_def.t _) first_num =
  let first_num = int_of_string first_num in
  loop first_num where rec loop p = do {
    let sol =
      loop_m [] 1 where rec loop_m rev_sol m =
        if m = p then List.rev rev_sol
        else
          let m3p1 =
            let m = num.of_int m in
            num.add_int (num.mul (num.mul m m) m) 1
          in
          let rev_sol =
            if num.mod_int m3p1 p = 0 then
              let k = num.div_int m3p1 p in
              [(k, m) :: rev_sol]
            else
            rev_sol
          in
          loop_m rev_sol (m + 1)
    in
    if List.length sol = 0 || not (Bnum.is_small_prime p) then ()
    else do {
      printf "%d" p;
      List.iter
        (fun (k, m) -> printf "\t%s.%d=%d%s+1" (num.to_string k) p m e3)
        sol;
      printf "\n";
      flush stdout;
    };
    loop (p + 1)
  }
;

value main () =
  let first_num = if Array.length Sys.argv = 2 then Sys.argv.(1) else "2" in
  what Bnum.mpz first_num
;

main ();

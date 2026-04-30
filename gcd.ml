(* $Id: gcd.ml,v 1.5 2010/01/21 09:11:58 deraugla Exp $ *)

open Bnum_def;
open Printf;

value gcd num = do {
  let x = Bnum.of_expr num (Istream.of_string Sys.argv.(1)) in
  let y = Bnum.of_expr num (Istream.of_string Sys.argv.(2)) in
  let r =
    loop 3 (num.gcd x y) where rec loop n g =
      if n >= Array.length Sys.argv then g
      else
        let e = Bnum.of_expr num (Istream.of_string Sys.argv.(n)) in
        loop (n + 1) (num.gcd g e)
  in
  printf "%s\n" (num.to_string r);
  flush stdout;
};

value main () = gcd Bnum.mpz;

main ();

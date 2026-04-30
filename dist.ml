(* $Id: dist.ml,v 1.1 2007/03/07 22:05:38 deraugla Exp $ *)
(* calcule la plus longue chaine de nombres composés *)

open Printf;

value is_prime n =
  if n mod 2 = 0 || n mod 3 = 0 then False
  else
    loop 5 2 where rec loop d dd =
      if n / d < d then True
      else if n mod d = 0 then False
      else loop (d + dd) (6 - dd)
;

value main p =
  loop 0 p p where rec loop dist last_p p =
    if is_prime p then
let _ = do { printf "%d..%d -> %d\027[K\r" last_p p (p - last_p); flush stdout; } in
      if p - last_p >= dist then
        let dist = p - last_p in
        do {
          printf "%d..%d -> %d\n" last_p p dist;
          flush stdout;
          loop dist p (p + 2)
        }
      else loop dist p (p + 2)
    else loop dist last_p (p + 1)
;

main (int_of_string Sys.argv.(1));

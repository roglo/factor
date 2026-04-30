(* $Id: modulo.ml,v 1.3 2003/06/17 17:41:11 ddr Exp $ *)
(* tous les a**i modulo p *)

open Printf;

value rec gcd a b = if b = 0 then a else gcd b (a mod b);

(**)
value quomod a b =
  (a / b, a mod b)
;

value power_mod a b p =
  loop 1 a b where rec loop res a b =
    if b = 0 then res
    else
      let (q, r) = quomod b 2 in
      if r = 0 then
        loop res ((a * a) mod p) q
      else
        loop ((res * a) mod p) ((a * a) mod p) q
;
(**)

value pow_modulo p =
  let phi_p =
    loop 0 1 where rec loop r a =
      if a >= p then r
      else if gcd a p = 1 then loop (r + 1) (a + 1)
      else loop r (a + 1)
  in
  for a = 1 to p - 1 do {
    if gcd a p = 1 then
      do {
        loop 1 a where rec loop j v =
          if j > phi_p then ()
          else
            do {
              printf " %2d" v;
              loop (j + 1) (v * a mod p);
            };
        printf "\n";
        flush stdout;
      }
    else ();
  }
;

value mult_modulo p =
  for a = 1 to p - 1 do {
    loop 1 a where rec loop j v =
      if j = p then ()
      else
        do {
          printf " %2d" v;
          loop (j + 1) ((v + a) mod p);
        };
    printf "\n";
    flush stdout;
  }
;

(*
pow_modulo 561;
*)

(* $Id: count_primes.ml,v 1.3 2009/10/08 18:42:12 deraugla Exp $ *)

(**)
value k n i = if i * i > n then invalid_arg "k" else 1 + n - (n - n mod i)/i;
(**)

value h n i =
  Mpz.add_ui (Mpz.sub n (Mpz.div_q (Mpz.sub n (Mpz.div_r n i)) i)) 1
;

value is_prime n =
  if n < 2 then False
  else
    loop 2 where rec loop d =
      let q = n / d in
      if q < d then True
      else if n mod d = 0 then False
      else loop (d + 1)
;

value count n =
  let zn = Mpz.of_int n in
  let sqrt_n = truncate (sqrt (float n)) in
  loop (Mpz.of_int 1) 0 2 where rec loop accu pow i =
    if i > sqrt_n then
      if pow = 0 then Mpz.to_int accu
      else
        let p = Mpz.ui_pow_ui n (pow - 1) in
        Mpz.to_int (Mpz.div_q (Mpz.add accu (Mpz.div_q_ui p 2)) p) - 1
    else
      let (accu, pow) =
        if is_prime i then (Mpz.mul accu (h zn (Mpz.of_int i)), pow + 1)
        else (accu, pow)
      in
      loop accu pow (i + 1)
;

value count2 n =
  loop 0 2 where rec loop cnt i =
    if i > n then cnt
    else if is_prime i then loop (cnt + 1) (i + 1)
    else loop cnt (i + 1)
;

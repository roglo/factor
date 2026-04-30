(* $Id: phime.ml,v 1.4 2009/10/13 15:08:34 deraugla Exp $ *)
(* phime = phony prime *)

value is_prime_p n =
  if n <= 1 then False
  else
    loop 2 1 where rec loop d dd =
      if n / d < d then True
      else if n mod d = 0 then False
      else loop (d + dd) (if d <= 3 || dd = 4 then 2 else 4)
;

Random.self_init ();
value is_phime_p n =
  if n = 1 then False
  else
    let prob = 1. /. log (float n) in
    Random.float 1.0 < prob
;

value f pred n =
  loop [] 1 where rec loop rev_list i =
    if i < n then
      let rev_list = if pred i then [i :: rev_list] else rev_list in
      loop rev_list (i + 1)
    else List.rev rev_list
;

value fp = f is_prime_p;
value fh = f is_phime_p;

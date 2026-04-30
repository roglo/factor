(* $Id: conj4.ml,v 1.8 2017/12/28 10:59:45 deraugla Exp $ *)

open Printf;

module type Num =
  sig
    type t = 'abstract;
    value of_int : int -> t;
    value to_string : t -> string;
    value neg : t -> t;
    value sub : t -> t -> t;
    value mul_int : t -> int -> t;
    value mod_int : t -> int -> int;
    value sqr : t -> t;
    value sqrt : t -> t;
    value eq : t -> t -> bool;
    value lt_int : t -> int -> bool;
  end
;

(*
module Num_int : Num =
  struct
    type t = int;
    value of_int i = i;
    value to_string x = sprintf "%d" x;
    value neg x = -x;
    value sub x y = x - y;
    value sqr x = x * x;
    value sqrt x = truncate (sqrt (float x) +. 0.5);
    value eq x y = x = y;
    value lt_int x i = x < i;
  end
;
*)

module Num_mpz : Num =
  struct
    type t = Mpz.t;
    value of_int = Mpz.of_int;
    value to_string = Mpz.to_string 10;
    value neg = Mpz.neg;
    value sub = Mpz.sub;
    value mul_int = Mpz.mul_si;
    value mod_int = Mpz.div_r_ui;
    value sqr x = Mpz.mul x x;
    value sqrt = Mpz.sqrt;
    value eq x y = Mpz.compare x y = 0;
    value lt_int x i = Mpz.compare_si x i < 0;
  end
;

module Num = Num_mpz;

value is_utf8 =
  let lang =
    try Sys.getenv "LC_ALL" with
    [ Not_found ->
        try Sys.getenv "LC_MESSAGES" with
        [ Not_found ->
            try Sys.getenv "LANG" with
            [ Not_found -> "en" ] ] ]
  in
  match try Some (String.index lang '.') with [ Not_found -> None ] with
  [ Some i ->
      let s = String.sub lang (i + 1) (String.length lang - i - 1) in
      String.lowercase_ascii s = "utf-8"
  | None -> False ]
;

value e2 = if is_utf8 then "Â²" else "²";

value is_prime n =
  if n < 2 then False
  else
    loop 2 where rec loop d =
      let q = n / d in
      if q < d then True
      else if n mod d = 0 then False
      else loop (d + 1)
;

value main () =
  loop 2 where rec loop p = do {
    if is_prime p && p mod 4 = 1 then do {
      let d = 4 in
      let i =
        loop 1 2 2 where rec loop r i i_pow_r =
          if r > d then loop 1 (i + 1) (i + 1)
          else if i_pow_r = 1 then
            if r = d then i
            else loop 1 (i + 1) (i + 1)
          else
            let i_pow_r = Num.mul_int (Num.of_int i_pow_r) i in
            loop (r + 1) i (Num.mod_int i_pow_r p)
      in
      assert ((i * i + 1) mod p = 0);
      let k = (i * i + 1) / p in
      let a = p mod i in
      let a2 = Num.sqr (Num.of_int a) in
      let b2 = Num.sub (Num.of_int p) a2 in
      let s =
        if Num.lt_int b2 0 then
          sprintf "*** %d=%d%s-%s" p a e2 (Num.to_string (Num.neg b2))
        else do {
          let b = Num.sqrt b2 in
          assert (Num.eq (Num.sqr b) b2);
          sprintf "%d=%d%s+%s%s" p a e2 (Num.to_string b) e2
        }
      in
      let pad = max 1 (24 - String.length s) in
      printf "%s%si=%d\ti%s=%sp-1\n" s (String.make pad ' ')
        i e2 (if k = 1 then "" else string_of_int k);
      flush stdout;
    }
    else ();
    loop (p + 1)
  }
;

main ();

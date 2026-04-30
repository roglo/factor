(* $Id: bnum.mli,v 1.9 2010/01/21 09:11:58 deraugla Exp $ *)

value int : Bnum_def.t int;
value big : Bnum_def.t Big_int.big_int;
value mpz : Bnum_def.t Mpz.t;

value out_base : ref int;

value of_expr : Bnum_def.t 'a -> Istream.t char -> 'a;
value is_small_prime : int -> bool;
value divisible_by_small : Bnum_def.t 'a -> int -> 'a -> bool;
value small_divisor : Bnum_def.t 'a -> int -> 'a -> option int;
value random : Bnum_def.t 'a -> 'a -> 'a;
value to_string : Bnum_def.t 'a -> 'a -> string;
value to_short_string : Bnum_def.t 'a -> 'a -> string;

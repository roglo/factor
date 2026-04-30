(* $Id: fmr.mli,v 1.2 2009/12/06 10:04:06 deraugla Exp $ *)

type t 'a =
  [ Prime
  | Factor of list 'a and list 'a
  | Composite ]
;

value test : Bnum_def.t 'a -> 'a -> t 'a;
value is_probably_prime : Bnum_def.t 'a -> bool -> 'a -> bool;

(* ... *)

type probably_prime 'a =
  [ PrOne
  | PrMinusOne
  | PrBadSqrtOne of 'a
  | PrNotOne of 'a ]
;

type info 'a = 'abstract;

value init_fermat_test : Bnum_def.t 'a -> 'a -> info 'a;
value fermat_miller_rabin_test :
  Bnum_def.t 'a -> 'a -> info 'a -> 'a -> probably_prime 'a;

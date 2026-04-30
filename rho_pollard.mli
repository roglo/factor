(* $Id: rho_pollard.mli,v 1.1 2009/11/24 20:25:38 deraugla Exp $ *)

value max_it : ref int;
value factor_brent : Bnum_def.t 'a -> bool -> 'a -> option ('a * 'a);
value factor_floyd : Bnum_def.t 'a -> bool -> 'a -> option ('a * 'a);

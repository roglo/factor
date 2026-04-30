(* $Id: quad_sieve.mli,v 1.2 2009/11/25 01:44:56 deraugla Exp $ *)

value qs_search_cycles : ref bool;
value b_limit : ref int;
value factor : Bnum_def.t 'a -> bool -> 'a -> option ('a * 'a);

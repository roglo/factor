(* $Id: bnum_def.mli,v 1.14 2010/01/21 19:44:14 deraugla Exp $ *)

type randstate = { rs : unit -> unit };

type t 'a =
  { zero : 'a;
    one : 'a;
    two : 'a;
    of_int : int -> 'a;
    to_int : 'a -> int;
    of_float : float -> 'a;
    eq : 'a -> 'a -> bool;
    eq_int : 'a -> int -> bool;
    gt : 'a -> 'a -> bool;
    gt_int : 'a -> int -> bool;
    ge : 'a -> 'a -> bool;
    ge_int : 'a -> int -> bool;
    lt : 'a -> 'a -> bool;
    lt_int : 'a -> int -> bool;
    le : 'a -> 'a -> bool;
    le_int : 'a -> int -> bool;
    compare : 'a -> 'a -> int;
    abs : 'a -> 'a;
    neg : 'a -> 'a;
    add : 'a -> 'a -> 'a;
    add_int : 'a -> int -> 'a;
    sub : 'a -> 'a -> 'a;
    sub_int : 'a -> int -> 'a;
    mul : 'a -> 'a -> 'a;
    mul_int : 'a -> int -> 'a;
    div : 'a -> 'a -> 'a;
    div_int : 'a -> int -> 'a;
    mod_ : 'a -> 'a -> 'a;
    mod_int : 'a -> int -> int;
    quomod : 'a -> 'a -> ('a * 'a);
    quomod_int : 'a -> int -> ('a * int);
    submul : 'a -> 'a -> 'a -> 'a;
    submul_int : 'a -> 'a -> int -> 'a;
    addmul : 'a -> 'a -> 'a -> 'a;
    addmul_int : 'a -> 'a -> int -> 'a;
    power : 'a -> 'a -> 'a;
    power_int : 'a -> int -> 'a;
    power_int_int : int -> int -> 'a;
    power_mod : 'a -> 'a -> 'a -> 'a;
    power_mod_int : 'a -> int -> 'a -> 'a;
    log : 'a -> float;
    land_ : 'a -> 'a -> 'a;
    lor_ : 'a -> 'a -> 'a;
    lxor_ : 'a -> 'a -> 'a;
    sqrt : 'a -> 'a;
    min : 'a -> 'a -> 'a;
    gcd : 'a -> 'a -> 'a;
    randstate_self_init : unit -> randstate;
    randstate_init : 'a -> randstate;
    size_in_base : 'a -> int -> int;
    of_string : string -> 'a;
    to_string : 'a -> string }
;

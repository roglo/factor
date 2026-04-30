(* $Id: pseudofact.ml,v 1.7 2009/12/14 16:10:32 deraugla Exp $ *)

open Bnum_def;
open Printf;

value rec binome num n k =
  if k = 0 then num.one
  else num.div_int (num.mul_int (binome num n (k - 1)) (n - (k - 1))) k
;

value arg_fact = ref False;

value rec pseudofact (num : Bnum_def.t _) ht n =
  if n = 0 then num.one
  else
    match try Some (Hashtbl.find ht n) with [ Not_found -> None ] with
    [ Some r -> r
    | None -> do {
        let r =
          loop num.zero 0 where rec loop sum k =
            if k >= n then
              if n mod 2 = 1 then if arg_fact.val then sum else num.neg sum
              else sum
            else
              let uk =
                num.mul
                  (num.mul (binome num (n - 1) k) (pseudofact num ht k))
                  (pseudofact num ht (n - 1 - k))
              in
              loop (num.add sum uk) (k + 1)
        in
        Hashtbl.add ht n r;
        r
      } ]
;

value pseudofact_main num ht abs n = do {
  match n with
  [ Some n -> do {
      let r =
        let r = pseudofact num ht n in
        if abs then num.abs r else r
      in
      printf "%s\n" (num.to_string r);
      flush stdout;
    }
  | None -> do {
      loop 0 where rec loop n = do {
        let r =
          let r = pseudofact num ht n in
          if abs then num.abs r else r
        in
        printf "%d: %s\n" n (num.to_string r);
        flush stdout;
        loop (n + 1);
      }
    } ];
};

value arg_number = ref None;
value arg_abs = ref False;

value set_arg_number s = arg_number.val := Some (int_of_string s);
value speclist =
  Arg.align
    [("-abs", Arg.Set arg_abs, " Absolute value");
     ("-fact", Arg.Set arg_fact, " Factorial (not pseudo)")]
;
value usage =
  sprintf "usage: %s [options] Number\n"
    Sys.argv.(0)
;

value main () = do {
  Arg.parse speclist set_arg_number usage;
  pseudofact_main Bnum.mpz (Hashtbl.create 1) arg_abs.val arg_number.val;
};

main ();

(* $Id: gauss.ml,v 1.16 2009/12/12 00:53:42 deraugla Exp $ *)
(* primes in Gauss numbers *)

open Printf;

value gauss_norm (a, b) = a * a + b * b;

value gauss_to_string (a, b) =
  if b = 0 then sprintf "%d" a
  else if b = 1 then sprintf "%d+i" a
  else sprintf "%d+%di" a b
;

value rec gauss_succ (a, b) =
  let n = gauss_norm (a, b) in
  let (n, a, b) = if b = 0 then (n + 1, 1, a) else (n, a + 1, b - 1) in
  loop_a n a b 
and loop_a n a b =
  let n_minus_a2 = n - a * a in
  if n_minus_a2 < 0 then loop_a (n + 1) 1 a
  else loop_b n n_minus_a2 a b
and loop_b n n_minus_a2 a b =
  let b2 = b * b in
  if b2 < n_minus_a2 then loop_a n (a + 1) b
  else if b2 > n_minus_a2 then loop_b n n_minus_a2 a (b - 1)
  else (a, b)
;

value gauss_is_divisible_by (a, b) (c, d) =
  let ncd = gauss_norm (c, d) in
  (a * c + b * d) mod ncd = 0 && (b * c - a * d) mod ncd = 0
;

value gauss_first_divisor c =
  let nc = gauss_norm c in
  loop (1, 1) where rec loop d =
    let nd = gauss_norm d in
    if nd * nd > nc then None
    else if gauss_is_divisible_by c d then Some d
    else loop (gauss_succ d)
;

value gauss_is_prime c = gauss_first_divisor c = None;

value gauss_div (a, b) (c, d) =
(*
  a+ib   (a+ib)(c-id)   (ac+bd)+i(bc-ad)
  ---- = ------------ = ----------------
  c+id       c²+d²           c²+d²
*)
  let ncd = gauss_norm (c, d) in
  let (x, y) =
    let x = a * c + b * d in
    let y = b * c - a * d in
    if y < 0 then (-y, x) else (x, y)
  in
  (x / ncd, y / ncd)
;

value concat_dl rev_dl d =
  match rev_dl with
  [ [(d1, e1) :: rev_dl1] ->
      if d = d1 then [(d1, e1+1) :: rev_dl1]
      else [(d, 1) :: rev_dl]
  | [] -> [(d, 1)] ]
;

value gauss_factorize c =
  loop [] c (gauss_norm c) (1, 1) 2 where rec loop rev_dl c nc d nd =
    if nd * nd > nc then
      concat_dl rev_dl c
    else if gauss_is_divisible_by c d then
      let rev_dl = concat_dl rev_dl d in
      let q = gauss_div c d in
      loop rev_dl q (gauss_norm q) d nd
    else
      let d = gauss_succ d in
      loop rev_dl c nc d (gauss_norm d)
;

value gauss_primes n = do {
  printf "first %d normalized Gauss primes\n" n;
  printf "  normalized such that Re>0 and Im>=0\n";
  printf "  norm: N(c)=Re(c)^2+Im(c)^2 displayed between parentheses\n";
  printf "  order: c<d iff N(c)<N(d) or N(c)=N(d) and Re(c)<Re(d)\n";
  flush stdout;
  loop_cnt "" n (1, 1) where rec loop_cnt sep cnt c =
    if cnt <= 0 then ()
    else do {
      let cnt =
        match gauss_factorize c with
        [ [(_, 1)] -> do {
            printf "%s%s(%d)" sep (gauss_to_string c) (gauss_norm c);
            flush stdout;
            cnt - 1
          }
        | dl -> do {
            printf "%s%s" sep (gauss_to_string c);
            let _ =
              List.fold_left
                (fun sep (d, e) -> do {
                   let need_paren = snd d <> 0 in
                   printf "%s%s%s%s%s" sep (if need_paren then "(" else "")
                     (gauss_to_string d) (if need_paren then ")" else "")
                     (if e = 1 then "" else Stringexp.f e);
                   "."
                 })
                "=" (List.rev dl)
            in
            flush stdout;
            cnt
          } ]
      in
      loop_cnt "\n" cnt (gauss_succ c)
    };
  printf "\n";
  flush stdout;
};

value gauss_list n =
  loop "" n (0, 0) where rec loop sep n c =
    if n <= 0 then do {
      printf "\n";
      flush stdout
    }
    else do {
      printf "%s%s(%d)" sep (gauss_to_string c) (gauss_norm c);
      flush stdout;
      loop "\n" (n - 1) (gauss_succ c)
    }
;

value gauss arg_list n = if arg_list then gauss_list n else gauss_primes n;

value arg_list = ref False;

value speclist =
  Arg.align [("-l", Arg.Set arg_list, " List")]
;

value usage =
  sprintf "usage: %s [options] [Number]\n  \
   Options:"
    Sys.argv.(0)
;

value main () = do {
  let p = ref None in
  Arg.parse speclist (fun s -> p.val := Some s) usage;
  let n =
    match p.val with
    [ Some p -> int_of_string p
    | None -> max_int ]
  in
  gauss arg_list.val n
};

main ();

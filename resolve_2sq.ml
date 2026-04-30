(* $Id: resolve_2sq.ml,v 1.126 2010/06/04 07:51:20 deraugla Exp $ *)
(* Recursive resolving of Fermat's theorem on sums of two squares *)

open Printf;
open Bnum_def;

(*
#load "pa_pragma.cmo";
#load "pa_extend.cmo";
#load "q_ast.cmo";

#pragma
  EXTEND
    GLOBAL: expr;
    expr: LEVEL "+"
      [ [ e1 = SELF; "+:"; e2 = SELF -> <:expr< num.add $e1$ $e2$ >>
        | e1 = SELF; "-:"; e2 = SELF -> <:expr< $lid:"-:"$ $e1$ $e2$ >> ] ]
    ;
    expr: LEVEL "*"
      [ [ e1 = SELF; "*:"; e2 = SELF -> <:expr< $lid:"*:"$ $e1$ $e2$ >>
        | e1 = SELF; "/:"; e2 = SELF -> <:expr< $lid:"/:"$ $e1$ $e2$ >> ] ]
    ;
    expr: LEVEL "**"
      [ [ e1 = SELF; "**:"; e2 = SELF -> <:expr< $lid:"**:"$ $e1$ $e2$ >> ] ]
    ;
    expr: LEVEL "unary minus"
      [ [ "-:"; e = SELF -> <:expr< $lid:"-:~"$ $e$ >> ] ]
    ;
  END
;
*)

value verbose = ref False;
value e2 = Stringexp.e2;

value tab lev = String.make (2 + 2 * lev) ' ';

value search_square_root_of_minus_one num p =
  if num.eq_int p 2 then num.one
  else
    let p_minus_1 = num.sub_int p 1 in
    let p_minus_1_on_4 = num.div_int p_minus_1 4 in
    loop () where rec loop () =
      let m = num.add_int (Bnum.random num p_minus_1) 1 in
      let i = num.power_mod m p_minus_1_on_4 p in
      if num.eq_int i 1 || num.eq i p_minus_1 then loop ()
      else
        let p_i = num.sub p i in
        if num.lt i p_i then i else p_i
;

value factor_for_fmr_test = ref 1_000_000;
value max_small_factor = ref 10_000_000;

Rho_pollard.max_it.val := 1_000_000;
Quad_sieve.b_limit.val := 1500;
(*
factor_for_fmr_test.val := 1_000_000;
max_small_factor.val := 10_000_000;
*)
factor_for_fmr_test.val := 100_000;
max_small_factor.val := 200_000;
factor_for_fmr_test.val := 80_000;
max_small_factor.val := 100_000;
(**)

value factor_rho_pollard num n =
  match Rho_pollard.factor_floyd num False n with
  [ None -> Rho_pollard.factor_brent num False n
  | x -> x ]
;

type factor_result 'a 'b 'c =
  [ Succeeded of 'a
  | Has_factor_4n_plus_3 of 'b
  | Found_too_big_factor of 'c ]
;

value add_prime_or_factor num verbose mul_sign rev_fl nl d =
  if not (Fmr.is_probably_prime num False d) then
    Succeeded (rev_fl, [d :: nl])
  else do {
    if verbose then do {
      eprintf "%s%s" mul_sign (num.to_string d);
      flush stderr;
    }
    else ();
    if num.mod_int d 4 = 3 then Has_factor_4n_plus_3 d
    else Succeeded ([(d, 1) :: rev_fl], nl)
  }
;

value try_pollard num verbose rev_fl nl = do {
  if verbose then do {
    eprintf "(r)";
    flush stderr
  }
  else ();
  loop "" rev_fl nl where rec loop mul_sign rev_fl =
    fun
    [ [] -> Succeeded (List.rev rev_fl)
    | [n :: nl] ->
        match factor_rho_pollard num n with
        [ Some (d, n) ->
            if num.eq d n then do {
              if verbose then do {
                eprintf "%s%s^2" mul_sign (num.to_string d);
                flush stderr;
              }
              else ();
              loop "*" [(d, 2) :: rev_fl] nl
            }
            else
              match add_prime_or_factor num verbose mul_sign rev_fl nl d with
              [ Succeeded (rev_fl, nl) ->
                  match add_prime_or_factor num verbose "*" rev_fl nl n with
                  [ Succeeded (rev_fl, nl) -> loop "*" rev_fl nl
                  | Has_factor_4n_plus_3 f -> Has_factor_4n_plus_3 f
                  | Found_too_big_factor f -> Found_too_big_factor f ]
              | Has_factor_4n_plus_3 f -> Has_factor_4n_plus_3 f
              | Found_too_big_factor f -> Found_too_big_factor f ]
        | None -> Found_too_big_factor (rev_fl, [n :: nl]) ] ]
};

value try_sieve num verbose rev_fl nl = do {
  if verbose then do {
    eprintf "(q)";
    flush stderr
  }
  else ();
  loop rev_fl nl where rec loop rev_fl =
    fun
    [ [] -> Succeeded (List.rev rev_fl)
    | [n :: nl] ->
        match Quad_sieve.factor num False n with
        [ Some (d, n) ->
            if num.eq d n then do {
              if verbose then do {
                eprintf "*%s^2" (num.to_string d);
                flush stderr;
              }
              else ();
              loop [(d, 2) :: rev_fl] nl
            }
            else
              match add_prime_or_factor num verbose "*" rev_fl nl d with
              [ Succeeded (rev_fl, nl) ->
                  match add_prime_or_factor num verbose "*" rev_fl nl n with
                  [ Succeeded (rev_fl, nl) -> loop rev_fl nl
                  | Has_factor_4n_plus_3 f -> Has_factor_4n_plus_3 f
                  | Found_too_big_factor f -> Found_too_big_factor f ]
              | Has_factor_4n_plus_3 f -> Has_factor_4n_plus_3 f
              | Found_too_big_factor f -> Found_too_big_factor f ]
        | None -> Found_too_big_factor (rev_fl, [n :: nl]) ] ]
};

value limited_factorization num verbose fast n =
  (* factorization limited to prime factors :
      - 2
      - of the form 4n+1
      - of even powers of primes of the form 4n+3
     returns a value of type 'factor_result'.
     [fast] means don't insist on big factors.
   *)
  loop [] False 2 1 n where rec loop rev_fl fmr_tested d inc n =
    let (is_probably_prime, fmr_tested) =
      if not fmr_tested && d > factor_for_fmr_test.val then
        (Fmr.is_probably_prime num False n, True)
      else
        (False, fmr_tested)
    in
    let q = num.div_int n d in
    if is_probably_prime || num.lt_int q d then
      match rev_fl with
      [ [(d1, e1) :: rev_fl1] ->
          if num.eq_int n d1 then do {
            if verbose then do {
              eprintf "^%d" (e1+1);
              flush stderr;
            }
            else ();
            if d1 mod 4 = 3 && e1 mod 2 = 0 then
              Has_factor_4n_plus_3 (num.of_int d1)
            else
              let rev_fl = [(d1, e1+1) :: rev_fl1] in
              let rfl = List.map (fun (d, e) -> (num.of_int d, e)) rev_fl in
              Succeeded (List.rev rfl)
          }
          else do {
            if verbose then do {
              if e1 > 1 then eprintf "^%d" e1 else ();
              eprintf "*%s" (num.to_string n);
              flush stderr;
            }
            else ();
            if d1 mod 4 = 3 && e1 mod 2 = 1 then
              Has_factor_4n_plus_3 (num.of_int d1)
            else if num.mod_int n 4 = 3 then
              Has_factor_4n_plus_3 n
            else
              let rfl = List.map (fun (d, e) -> (num.of_int d, e)) rev_fl in
              Succeeded (List.rev [(n, 1) :: rfl])
          }
      | [] -> do {
          if num.mod_int n 4 = 3 then Has_factor_4n_plus_3 n
          else Succeeded [(n, 1)]
        } ]
    else if num.mod_int n d = 0 then
      match rev_fl with
      [ [(d1, e1) :: rev_fl1] ->
          if d = d1 then loop [(d1, e1+1) :: rev_fl1] False d inc q
          else do {
            if verbose then do {
              if e1 > 1 then eprintf "^%d" e1 else ();
              eprintf "*%d" d;
              flush stderr;
            }
            else ();
            if d1 mod 4 = 3 && e1 mod 2 = 1 then
              Has_factor_4n_plus_3 (num.of_int d1)
            else
              loop [(d, 1) :: rev_fl] False d inc q
          }
      | [] -> do {
          if verbose then do {
            eprintf "=%d" d;
            flush stderr;
          }
          else ();
          loop [(d, 1)] False d inc q
        } ]
    else if d > max_small_factor.val then
      let fact_4n_p_3 =
        match rev_fl with
        [ [(d, e) :: _] -> do {
            if verbose then do {
              if e > 1 then eprintf "^%d" e else ();
              eprintf "*";
              flush stderr;
            }
            else ();
            if d mod 4 = 3 && e mod 2 = 1 then Some d else None
          }
        | [] -> do {
            if verbose then do {
              eprintf "=";
              flush stderr;
            }
            else ();
            None
          } ]
      in
      match fact_4n_p_3 with
      [ Some n -> Has_factor_4n_plus_3 (num.of_int n)
      | None ->
          if fast then Found_too_big_factor n
          else
            let rev_fl = List.map (fun (d, e) -> (num.of_int d, e)) rev_fl in
            match try_pollard num verbose rev_fl [n] with
            [ Succeeded fl -> Succeeded fl
            | Has_factor_4n_plus_3 n -> Has_factor_4n_plus_3 n
            | Found_too_big_factor (rev_fl, nl) ->
                match try_sieve num verbose rev_fl nl with
                [ Succeeded fl ->
                    Succeeded fl
                | Has_factor_4n_plus_3 n ->
                    Has_factor_4n_plus_3 n
                | Found_too_big_factor (rev_fl, nl) ->
                    Found_too_big_factor (List.hd nl) ] ] ]
    else
      let new_inc = if inc = 2 then 4 else if inc = 1 then -2 else 2 in
      loop rev_fl fmr_tested (d + abs inc) new_inc n
;

value diophante num (a, b) (c, d) =
  (* (a²+b²)(c²+d²) = (ad+bc)²+(ac-bd)² = (ad-bc)²+(ac+bd)² *)
  let ac = num.mul a c in
  let ad = num.mul a d in
  let bc = num.mul b c in
  let bd = num.mul b d in
  let x1 = num.add ad bc in
  let y1 = if num.gt ac bd then num.sub ac bd else num.sub bd ac in
  let x2 = if num.gt ad bc then num.sub ad bc else num.sub bc ad in
  let y2 = num.add ac bd in
  ((x1, y1), (x2, y2))
;

value rec list_uniq eq =
  fun
  [ [x :: l] ->
      let l = list_uniq eq l in
      match l with
      [ [y :: _] -> if eq x y then l else [x :: l]
      | [] -> [x] ]
  | [] -> [] ]
;

value has_small_divisors num n = Bnum.divisible_by_small num 20000 n;

value is_probably_prime num n =
  if num.le_int n 1 then False
  else if num.le_int n max_int && Bnum.is_small_prime (num.to_int n) then True
  else if has_small_divisors num n then False
  else Fmr.is_probably_prime num False n
;

value rec next_prime num x =
  if is_probably_prime num x then x else next_prime num (num.add_int x 1)
;

value total_retries = ref 0;

value rec resolve_2sq_for_composed num lev u n retries = do {
  if verbose.val then do {
    eprintf "%sresolving x%s+y%s=%s" (tab (lev - 1)) e2 e2 (num.to_string n);
    flush stderr;
  }
  else ();
  match limited_factorization num verbose.val (lev > 0) n with
  [ Has_factor_4n_plus_3 n -> do {
      if verbose.val then do {
        eprintf "...\n";
        flush stderr;
      }
      else ();
      Has_factor_4n_plus_3 n
    }
  | Found_too_big_factor n -> do {
      if verbose.val then do {
        eprintf "...\n";
        flush stderr;
      }
      else ();
      Found_too_big_factor n
    }
  | Succeeded fl -> do {
      if verbose.val then do {
        eprintf "\n";
        if num.gt_int u 1 then do {
          let ndig = num.size_in_base n 10 in
          eprintf "%sfactorization successful" (tab (lev - 1));
          if ndig >= 10 then do {
            eprintf " (%s)"
              (match fl with
               [ [(_, 1)] -> "prime number"
               | _ -> sprintf "%d factors" (List.length fl) ]);
            eprintf " after %d %s" retries
              (if retries = 1 then "retry" else "retries");
          }
          else ();
          let su = num.to_string u in
          eprintf " for ((%si mod p)%s+%s%s)/p" su e2 su e2;
          if ndig >= 10 then eprintf " (%d digits)" ndig else ();
          eprintf "\n";
        }
        else ();
        flush stderr;
      }
      else ();
      let rl =
        loop [] fl where rec loop rev_rl =
          fun
          [ [(p, e) :: fl] ->
               if num.mod_int p 4 = 3 && e mod 2 = 0 then
                 let sol = (p, num.zero) in
                 loop [(sol, e / 2) :: rev_rl] fl
               else do {
                 if verbose.val then do {
                   eprintf "%s* prime resolving x%s+y%s=%s\n" (tab (lev - 1))
                     e2 e2 (num.to_string p);
                   flush stderr;
                 }
                 else ();
                 match resolve_2sq_for_prime num (lev + 1) p with
                 [ Succeeded sol -> do {
                     if verbose.val then do {
                       let (x1, y1) = sol in
                       eprintf "%sprime resolved %s%s+%s%s=%s\n" (tab lev)
                         (num.to_string x1) e2 (num.to_string y1) e2
                         (num.to_string p);
                       flush stderr
                     }
                     else ();
                     loop [(sol, e) :: rev_rl] fl
                   }
                 | Has_factor_4n_plus_3 _ -> assert False
                 | Found_too_big_factor n -> Found_too_big_factor n ]
               }
          | [] ->
              Succeeded rev_rl ]
      in
      match rl with
      [ Succeeded rl ->
          let compare_sol (x1, _) (x2, _) = num.compare x2 x1 in
          let sort_sol (x, y) = if num.gt x y then (x, y) else (y, x) in
          loop [] rl where rec loop sol_list =
            fun
            [ [(sol1, e) :: rl] ->
                let rl = if e = 1 then rl else [(sol1, e-1) :: rl] in
                let sl =
                  if sol_list = [] then [sort_sol sol1]
                  else
                    let sl =
                      List.fold_left
                        (fun sl sol2 ->
                           let (sol1, sol2) = diophante num sol1 sol2 in
                           [sort_sol sol1; sort_sol sol2 :: sl])
                         [] sol_list
                    in
                    let sl = List.sort compare_sol sl in
                    list_uniq (fun (x1, _) (x2, _) -> num.eq x1 x2) sl
                in
                loop sl rl
            | [] -> Succeeded sol_list ]
      | Has_factor_4n_plus_3 _ -> assert False
      | Found_too_big_factor n -> Found_too_big_factor n ]
    } ]
}

and resolve_2sq_for_prime num lev p =
  let i = search_square_root_of_minus_one num p in
  let max_retries = 1000 in
  loop num.one 0 where rec loop v retries =
    match resolve_2sq_for_prime_with_param num lev p i v retries with
    [ Found_too_big_factor n ->
        let v =
          if num.eq_int v 1 then
            (* values before p/i not interesting because the computed k
               is just k*u², not smoother than k alone *)
            let v = num.add_int (num.div p i) 1 in
            if num.gt_int v 2 then v else num.two
          else
            (* taking only primes as multiplicators to avoid having
               harmonic k values and therefore having to test them *)
            next_prime num (num.add_int v 1)
        in
        if retries > max_retries then Found_too_big_factor n
        else do {
          if verbose.val then do {
            eprintf "%stoo big factor; trying with u=%s...\n" (tab lev)
              (num.to_string v);
            flush stderr;
          }
          else ();
          incr total_retries;
          loop v (retries + 1)
        }
    | Has_factor_4n_plus_3 x -> Has_factor_4n_plus_3 x
    | Succeeded x -> Succeeded x ]

and resolve_2sq_for_prime_with_param num lev p i v retries = do {
  let u = if num.le p v then num.one else v in
  let (i, k) = do {
    let i =
      let i = num.mod_ (num.mul i u) p in
      let p_i = num.sub p i in
      if num.le i p_i then i else p_i
    in
    let i2 = num.mul i i in
    assert (num.gt_int i2 0);
    let i2_plus_u2 = num.add i2 (num.mul u u) in
    let k = num.div i2_plus_u2 p in
    if verbose.val then do {
      eprintf "%si=%s i%s=%sp-%s\n" (tab lev) (num.to_string i) e2
        (if num.eq_int k 1 then "" else num.to_string k)
        (num.to_string (num.mul u u));
      flush stderr;
    }
    else ();
    assert (num.eq_int (num.mod_ i2_plus_u2 p) 0);
    (i, k)
  }
  in
  if num.eq_int i 0 then Succeeded (num.one, num.one)
  else if num.eq_int k 1 then Succeeded (i, u)
  else
    match resolve_2sq_for_composed num (lev + 1) u k retries with
    [ Succeeded sol_list ->
        loop sol_list where rec loop =
          fun
          [ [(x, y) :: sol_list] -> do {
              assert (num.ge_int x 0 && num.ge_int y 0);
              assert (num.eq (num.add (num.mul x x) (num.mul y y)) k);
              let sol =
                let ux = num.mul x u in
                let uy = num.mul y u in
                if num.eq_int (num.mod_ (num.addmul ux i y) k) 0 then
                  let x1 = num.div (num.submul uy i x) k in
                  let y1 = num.div (num.addmul ux i y) k in
                  Some (num.abs x1, num.abs y1, True)
                else if num.eq_int (num.mod_ (num.addmul uy i x) k) 0 then
                  let x1 = num.div (num.addmul uy i x) k in
                  let y1 = num.div (num.submul ux i y) k in
                  Some (num.abs x1, num.abs y1, False)
                else
                  None
              in
              match sol with
              [ Some (x1, y1, first_case) -> do {
                  if verbose.val then do {
                    eprintf "%ssolution %s%s+%s%s=%s\n" (tab lev)
                      (num.to_string x) e2 (num.to_string y) e2
                      (num.to_string k);
                    eprintf "%swhich divides " (tab lev);
                    let (x, y) = if first_case then (x, y) else (y, x) in
                    eprintf "%s+%si" (num.to_string x)
                      (if num.eq_int y 1 then "" else num.to_string y);
                    eprintf " (i=%s)\n" (num.to_string i);
                    flush stderr;
                  }
                  else ();
                  Succeeded (x1, y1)
                }
              | None -> loop sol_list ]
            }
          | [] -> assert False ]
    | Has_factor_4n_plus_3 _ -> assert False
    | Found_too_big_factor n -> Found_too_big_factor n ]
};

value resolve_2sq num lev n =
  if num.le_int n 1 then Succeeded [(n, num.zero)]
  else resolve_2sq_for_composed num lev num.zero n 0
;

value from = ref False;
value next_working = ref False;
value only_primes = ref False;

value resolve_2sq_main num p =
  match (p, from.val) with
  [ (Some s, False) ->
      let n =
        let n = num.abs (Bnum.of_expr num (Istream.of_string s)) in
        if next_working.val && num.ge_int n 3 then do {
          let saved_rho_max_it = Rho_pollard.max_it.val in
          let saved_sieve_limit = Quad_sieve.b_limit.val in
          Rho_pollard.max_it.val := 100_000;
          Quad_sieve.b_limit.val := 800;
          let restore_global () = do {
            Rho_pollard.max_it.val := saved_rho_max_it;
            Quad_sieve.b_limit.val := saved_sieve_limit;
          }
          in 
          loop True n where rec loop first n =
            let test =
              if only_primes.val then
                if is_probably_prime num n && num.mod_int n 4 = 1 then
                  Succeeded []
                else
                  Has_factor_4n_plus_3 n
              else limited_factorization num False False n
            in
            match test with
            [ Succeeded _ -> do {
                if verbose.val then do {
                  if not first then eprintf "\n" else ();
                  eprintf "%s\n" (num.to_string n);
                  flush stderr;
                }
                else ();
                restore_global ();
                n
              }
            | Has_factor_4n_plus_3 _ -> do {
                if verbose.val then do {
                  if first then do {
                    eprintf "%s\n" (num.to_string n);
                    flush stderr;
                  }
                  else ();
                  eprintf ".";
                  flush stderr;
                }
                else ();
                loop False (num.add_int n 1)
              }
            | Found_too_big_factor _ -> do {
                if verbose.val then do {
                  if first then do {
                    eprintf "%s\n" (num.to_string n);
                    flush stderr;
                  }
                  else ();
                  eprintf "*";
                  flush stderr;
                }
                else ();
                loop False (num.add_int n 1)
              } ]
        }
        else
          n
      in
      match resolve_2sq num 0 n with
      [ Succeeded sol_list -> do {
          if not verbose.val then printf "%s" (num.to_string n) else ();
          List.iter
            (fun (x, y) -> do {
               if verbose.val then printf "%s" (num.to_string n) else ();
               printf "=%s%s+%s%s" (num.to_string x) e2 (num.to_string y) e2;
               if verbose.val then printf "\n" else ();
               flush stdout;
               assert (num.eq n (num.add (num.mul x x) (num.mul y y)));
             })
            sol_list;
          if verbose.val then
            if total_retries.val > 0 then
              printf "total number of retries %d\n" total_retries.val
            else ()
          else
            printf "\n";
          flush stdout;
        }
      | Has_factor_4n_plus_3 n -> do {
          let s = num.to_string n in
          printf "no solution: ";
          if String.length s > 8 then do {
            printf "has a factor of the form 4n+3 at an odd power:\n";
            printf "  %s\n" s;
          }
          else
            printf "has a factor (%s) of the form 4n+3 at an odd power\n" s;
          flush stdout;
          exit 1
        }
      | Found_too_big_factor n -> do {
          printf "failed: encountered a too big factor: %s\n"
            (num.to_string n);
          flush stdout;
          exit 2
        } ]
  | (Some _, True) | (None, _) ->
      let first_num =
        match p with
        [ Some s -> num.abs (Bnum.of_expr num (Istream.of_string s))
        | None -> num.one ]
      in
      loop first_num where rec loop n = do {
        let test =
          if only_primes.val then
            if is_probably_prime num n && num.mod_int n 4 = 1 then do {
              if verbose.val then do {
                eprintf "\n";
                flush stderr;
              }
              else ();
              Succeeded [(n, 1)]
            }
            else do {
              if verbose.val then do {
                eprintf ".";
                flush stderr;
              }
              else ();
              Has_factor_4n_plus_3 n
            }
          else
            limited_factorization num False False n
        in
        match test with
        [ Succeeded dl -> do {
            if verbose.val then eprintf "%s\n" (num.to_string n) else ();
            flush stderr;
            match resolve_2sq num 0 n with
            [ Succeeded sol_list -> do {
                if verbose.val then () else printf "%s" (num.to_string n);
                flush stdout;
                List.iter
                  (fun (x, y) -> do {
                     if verbose.val then printf "%s" (num.to_string n) else ();
                     printf "=%s%s+%s%s" (num.to_string x) e2 (num.to_string y)
                       e2;
                     if verbose.val then printf "\n" else ();
                     flush stdout;
                     assert (num.eq n (num.add (num.mul x x) (num.mul y y)));
                   })
                  sol_list;
                  if verbose.val then
                    if total_retries.val > 0 then
                      printf "total number of retries %d\n" total_retries.val
                    else ()
                  else ();
                  printf "\n";
                  flush stdout;
              }
            | Has_factor_4n_plus_3 _ | Found_too_big_factor _ -> () ];
          }
        | Has_factor_4n_plus_3 _ | Found_too_big_factor _ -> () ];
        loop (num.add_int n 1)
      } ]
;

value num_kind = ref "mpz";

value arg_number = ref None;
value set_arg_number s = arg_number.val := Some s;

value speclist =
  Arg.align
    [("-b", Arg.Unit (fun () -> num_kind.val := "big"),
      " Use ocaml big int library");
     ("-f", Arg.Set from, " List all numbers from that number on");
     ("-i", Arg.Unit (fun () -> num_kind.val := "int"),
      " Use native integers");
     ("-p", Arg.Set only_primes, " Search (-s) or List (-f) only primes");
     ("-s", Arg.Set next_working, " Search next working number");
     ("-v", Arg.Set verbose, " Verbose");
     ("-", Arg.String set_arg_number, "")]
;
value usage =
  sprintf "usage: %s [options] - [Number]\n  \
   If no Number, list all Numbers starting from 1\n  Options:"
    Sys.argv.(0);

value main () = do {
  Arg.parse speclist set_arg_number usage;
  match num_kind.val with
  [ "big" -> resolve_2sq_main Bnum.big arg_number.val
  | "int" -> resolve_2sq_main Bnum.int arg_number.val
  | "mpz" -> resolve_2sq_main Bnum.mpz arg_number.val
  | _ -> assert False ]
};

main ();

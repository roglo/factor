(* $Id: factor.ml,v 1.231 2019/06/12 11:58:00 deraugla Exp $ *)

open Printf;
open Bnum_def;

value verbose = ref False;
value test_small = ref True;
value test_phime = ref False;
value quiet = ref False;

value list_mapi f l =
  loop 0 l where rec loop i =
    fun
    [ [x :: l] ->
        let y = f i x in
        [y :: loop (i + 1) l]
    | [] -> [] ]
;

value rec list_up_extract x =
  fun
  [ [] -> None
  | [(a, b) :: l] ->
      if x = a then Some (b, l)
      else if x < a then None
      else list_up_extract x l ]
;

type not_gcd 'a 'b =
  [ Result of 'a
  | GCD_found of 'b ]
;

value rec solve_axpby_eq_1 num a b =
  if num.eq_int b 0 then (num.one, num.zero)
  else
    let (q, r) = num.quomod a b in
    let (x, y) = solve_axpby_eq_1 num b r in
    (y, num.sub x (num.mul q y))
;

value inv_big_int_mod num a b =
  let g = num.gcd a b in
  if num.eq_int g 1 then Result (num.mod_ (fst (solve_axpby_eq_1 num a b)) b)
  else GCD_found g
;

value rec gcd p q = if q = 0 then p else gcd q (p mod q);

value rec solve_axpby_eq_1_int a b =
  if b = 0 then (1, 0)
  else
    let (q, r) = (a / b, a mod b) in
    let (x, y) = solve_axpby_eq_1_int b r in
    (y, x + q * y)
;

value inv_mod num a b =
  let g = gcd a b in
  if g = 1 then Result (fst (solve_axpby_eq_1_int a b) mod b)
  else GCD_found (num.of_int g)
;

value ext_mult_big_int num gadd gzero n a =
  loop gzero n a where rec loop res n a =
    if num.eq_int n 0 then res
    else
      let (q, r) = num.quomod_int n 2 in
      if r = 0 then loop res q (gadd a a)
      else loop (gadd res a) q (gadd a a)
;

value power_big_int_mod num a b p =
  ext_mult_big_int num (fun x y -> num.mod_ (num.mul x y) p) num.one b a
;

value string_of_big_pi num pi = do {
  let s = Bnum.to_string num pi in
  let b = Buffer.create 1 in
  Buffer.add_char b s.[0];
  Buffer.add_char b '.';
  loop 5 1 where rec loop cntdig i =
    if i = String.length s then Buffer.contents b
    else do {
      let cntdig =
        if cntdig = 0 then do { Buffer.add_char b '_'; 4 }
        else cntdig - 1
      in
      Buffer.add_char b s.[i];
      loop cntdig (i + 1)
    }
};

(* m*atan(1/x) *)
value mult_atan_inv num m x =
  let y = num.div_int m x in
  let y2 = num.div_int y x in
  loop y y 3 where rec loop sum un1 n =
    let un = num.neg (num.div (num.mul un1 y2) m) in
    if num.eq_int un 0 then sum
    else loop (num.add sum (num.div_int un n)) un (n + 2)
;

value compute_pi num ndec = do {
  let pow = num.power (num.of_int 10) ndec in
  let pi =
    num.mul_int
(*
      (* 4 atan(1/5) - atan(1/239) *)
      (num.sub (num.mul_int 4 (mult_atan_inv pow 5))
         (mult_atan_inv pow 239))
*)
      (* 44 atan(1/57) + 7 atan(1/239) - 12 atan(1/682) + 24 atan(1/12943) *)
      (num.add
         (num.sub
            (num.add
               (num.mul_int (mult_atan_inv num pow 57) 44)
               (num.mul_int (mult_atan_inv num pow 239) 7))
            (num.mul_int (mult_atan_inv num pow 682) 12))
         (num.mul_int (mult_atan_inv num pow 12943) 24))
      4
(**)
  in
  printf "%s\n" (string_of_big_pi num pi);
  flush stdout;
};

(* Ramanujan :
                                9801
   pi = ---------------------------------------------
                    infinity  (4n)!  1103 + 26390 n
         2 sqrt 2 sum        ------ ----------------
                    n=0       n!^4     (4 x 99)^4n
*)

(**)
value compute_pi num ndec = do {
  let pow = num.power (num.of_int 100) ndec in
  let big_26390 = num.of_int 26390 in
  let big_4_99 = num.of_int (4 * 99) in
  let pi =
    let sum =
      loop num.zero 0 pow where rec loop r n vn =
        let un =
          num.mul vn (num.add_int (num.mul_int big_26390 n) 1103)
        in
        if num.eq_int un 0 then r
        else
          let vn1 =
            let m_4_np1 =
              num.mul_int
                (num.mul_int
                   (num.mul_int (num.of_int (4 * n + 4)) (4 * n + 3))
                   (4 * n + 2))
                (4 * n + 1)
            in
            num.div (num.mul vn m_4_np1)
              (num.power_int (num.mul_int big_4_99 (n + 1)) 4)
          in
          loop (num.add r un) (n + 1) vn1
    in
    let sqrt_2 = num.sqrt (num.mul_int pow 2) in
    let two_sqrt_2_sum = num.mul_int (num.mul sqrt_2 sum) 2 in
    num.div (num.mul pow (num.mul_int pow 9801)) two_sqrt_2_sum
  in
  printf "%s\n" (string_of_big_pi num pi);
  flush stdout;
};
(**)

value syracuse_sequence num m =
  loop "" [] 1 m where rec loop sep prev len n =
    if List.exists (num.eq n) prev then do {
      printf "%s%s\n" sep (Bnum.to_string num n);
      printf "length %d\n" len;
      flush stdout
    }
(*
    else if num.mod_int n 2 = 0 then loop sep len (num.div_int n 2)
    else do {
      printf "%s%s" sep (Bnum.to_string n);
      flush stdout;
      let n = num.add_int (num.mul_int 3 n) 1 in
      loop " " (len + 1) (num.div_int n 2)
    }
*)
(**)
    else do {
      printf "%s%s" sep (Bnum.to_string num n);
      flush stdout;
      let next_n =
        if num.mod_int n 2 = 0 then num.div_int n 2
        else num.add_int (num.mul_int n 3) 1
      in
      loop " " [n :: prev] (len + 1) next_n
    }
(*
    else do {
      printf "%s%s" sep (Bnum.to_string n);
      flush stdout;
      let next_n =
        if num.mod_int n 4 = 0 then num.div_int n 4
        else if num.mod_int n 4 = 1 then num.add_int 3 (num.mul_int 5 n)
        else if num.mod_int n 4 = 2 then num.add_int 2 (num.mul_int 5 n)
        else num.add_int (num.mul_int 5 n) 1
      in
      loop " " [n :: prev] (len + 1) next_n
    }
*)
;

value just_factor_nnl num gn =
  let n = gn.val in
  let _ = do { printf "%s:" (Bnum.to_string num n); flush stdout } in
  let flush_pow n pow d = do {
    let (n, pow) = if num.eq n d then (num.one, pow + 1) else (n, pow) in
    if pow > 1 then do { printf "%s" (Stringexp.f pow); flush stdout }
    else ();
    n
  }
  in
  if num.le_int n 2 then do {
    printf " %s" (Bnum.to_string num n);
    flush stdout
  }
  else
    let rec loop cnt n d dd pow perhaps_prime =
      let _ = gn.val := n in
      let (q, r) = num.quomod n d in
      if num.lt q d then do {
        let n = flush_pow n pow d in
        if num.eq_int n 1 then ()
        else printf " %s" (Bnum.to_string num n);
        if verbose.val then printf "\027[K" else ();
        flush stdout
      }
      else if num.eq_int r 0 then do {
        if pow = 0 then do {
          printf " %s" (Bnum.to_string num d);
          if verbose.val then printf "\027[K" else ();
          flush stdout
        }
        else ();
        loop 16666 q d dd (pow + 1) True
      }
      else
        let cnt =
          if cnt < 0 then
            let _ =
              if verbose.val then do {
                let s =
                  sprintf "(%s - %d digits)" (Bnum.to_string num d)
                    (String.length (Bnum.to_string num n))
                in
                eprintf "%s%s" s (String.make (String.length s) '\b');
                flush stderr
              }
              else ()
            in
            16666
          else cnt - 1
        in
        let n = flush_pow n pow d in
        if cnt = 1 && perhaps_prime then
          match Fmr.test num n with
          [ Fmr.Prime -> do {
              printf " %s*" (Bnum.to_string num n);
              if verbose.val then printf "\027[K" else ();
              flush stdout
            }
          | Fmr.Factor pl1 pl2 -> do {
              List.iter (fun n -> printf " %s." (Bnum.to_string num n)) pl1;
              flush stdout;
              match pl2 with
              [ [] -> ()
              | [p2] ->
                  loop cnt p2 (num.add_int d dd)
                    (if dd = 2 && num.gt_int d 4 then 4 else 2) 0 False
              | pl2 -> do {
                  printf "\nPlusieurs composés:";
                  List.iter (fun n -> printf " %s" (Bnum.to_string num n)) pl2;
                  flush stdout
                } ]
            }
          | Fmr.Composite ->
              loop cnt n (num.add_int d dd)
                (if dd = 2 && num.gt_int d 4 then 4 else 2) 0 False ]
        else
          loop cnt n (num.add_int d dd)
            (if dd = 2 && num.gt_int d 4 then 4 else 2) 0 perhaps_prime
    in
    loop 0 n num.two 1 0 True
;

value just_factor num gn = do {
  just_factor_nnl num gn;
  printf "\n";
  flush stdout
};

value factor num n =
(*
let _ = Printf.eprintf "Sys.catch_break true\n%!" in
*)
  let _ = Sys.catch_break True in
  let n = num.abs n in
  let gn = ref n in
  try just_factor num gn with
  [ Sys.Break -> do {
let _ = Printf.eprintf "Interrupted\n%!" in
      let s = Bnum.to_string num gn.val in
      printf "\n*** stop factorization %s (%d digits)\n" s (String.length s);
      flush stdout;
      if Fmr.is_probably_prime num True gn.val then
        printf " probably prime\n"
      else
        printf " composed\n";
      flush stdout
    } ]
;

value big_approx_log num n = float (num.size_in_base n 2 - 1) *. log 2.;

value is_phime num n =
  if num.eq_int n 1 then False
  else
    let prob = 1. /. big_approx_log num n in
    Random.float 1.0 < prob
;

value rec search_prime num n =
  if test_phime.val then
    if is_phime num n then n else search_prime num (num.add_int n 1)
  else if
    test_small.val && Bnum.divisible_by_small num 20000 n ||
    num.mod_int n 2 = 0
  then
    search_prime num (num.add_int n 1)
  else do {
    if not quiet.val then do {
      printf "\n*** %s\n" (Bnum.to_short_string num n);
      flush stdout
    }
    else ();
    if Fmr.is_probably_prime num (not quiet.val) n then n
    else search_prime num (num.add_int n 2)
  }
;

type accu 'a =
  { accu_n : mutable 'a;
    accu_cnt : mutable int;
    accu_delta : mutable int;
    accu_min_delta : mutable int;
    accu_max_delta : mutable int }
;

value rec find_prime_aux num n accu = do {
  let p = search_prime num (num.add_int n 1) in
  if not quiet.val then do {
    printf "Trouvé!\n";
    printf "%s\n" (Bnum.to_string num p)
  }
  else ();
  let delta = num.sub p n in
  let int_delta = num.to_int delta in
  printf "(number + %s)" (Bnum.to_string num delta);
  if accu.accu_cnt <> 0 then do {
    accu.accu_delta := accu.accu_delta + int_delta;
    accu.accu_min_delta := min int_delta accu.accu_min_delta;
    accu.accu_max_delta := max int_delta accu.accu_max_delta;
    printf "\t(ave %d min %d max %d)" (accu.accu_delta / accu.accu_cnt)
      accu.accu_min_delta accu.accu_max_delta
  }
  else ();
  accu.accu_cnt := accu.accu_cnt + 1;
  accu.accu_n := p;
  printf " (log ~= %g)" (big_approx_log num p);
  printf "\n";
  flush stdout;
  if quiet.val then find_prime_aux num p accu else ()
};

value find_prime num n =
  if num.lt_int n 3 then failwith "does not work for n < 3"
  else
    let _ = Sys.catch_break True in
    let accu =
      {accu_n = n; accu_cnt = 0; accu_delta = 0; accu_min_delta = max_int;
       accu_max_delta = 0}
    in
    try do {
      printf "%s\n" (Bnum.to_string num n);
      flush stdout;
      find_prime_aux num n accu
    }
    with
    [ Sys.Break -> do {
        printf "\n%s\n" (Bnum.to_string num accu.accu_n);
        flush stdout
      } ]
;

value test_fermat_miller_rabin num nl =
  let (n, a) =
    match nl with
    [ [a; n] -> (n, a)
    | _ ->
        let n = List.hd nl in
        let a = num.add_int (Bnum.random num (num.sub_int n 3)) 2 in
        (n, a) ]
  in
  let _ = if num.mod_int n 2 = 0 then failwith "even" else () in
  let _ = do { printf "%s" (Bnum.to_short_string num a); flush stdout } in
  let info = Fmr.init_fermat_test num n in
  match Fmr.fermat_miller_rabin_test num n info a with
  [ Fmr.PrOne -> do { printf " -> 1\n"; flush stdout }
  | Fmr.PrMinusOne -> do { printf " -> -1 ...\n"; flush stdout }
  | Fmr.PrBadSqrtOne v -> do {
      printf " -> mauvaise racine de 1: %s\n" (Bnum.to_string num v);
      flush stdout
    }
  | Fmr.PrNotOne v -> do {
      printf " -> %s\n" (Bnum.to_short_string num v);
      flush stdout
    } ]
;

value carmichael num n =
  let n = if num.mod_int n 2 = 0 then num.add_int n 1 else n in
  loop n where rec loop n = do {
    let _ = do {
      let s = sprintf "(%s)" (Bnum.to_string num n) in
      printf "%s%s" s (String.make (String.length s) '\b');
      flush stdout
    }
    in
    let is_carmichael =
      loop 2 False num.two 1 where rec loop ntimes div_found d dd =
        let (q, r) = num.quomod n d in
        if num.lt q d then div_found && ntimes <= 0
        else if ntimes <= 0 && div_found then div_found
        else
          let ndd = if dd = 4 || num.le_int d 3 then 2 else 4 in
          if num.eq_int r 0 then loop ntimes True (num.add_int d dd) ndd
          else
            let a = num.gcd n d in
            if num.eq_int a 1 then
              if num.eq_int (power_big_int_mod num d (num.sub_int n 1) n) 1
              then
                loop (ntimes - 1) div_found (num.add_int d dd) ndd
              else False
            else
              (*
              loop ntimes div_found (num.add d dd) ndd
              *)
              loop ntimes True (num.add_int d dd) ndd
    in
    if is_carmichael then do {
      printf " %s" (Bnum.to_string num n);
      flush stdout
    }
    else ();
    loop (num.add_int n 2)
  }
;

value prime_on_twin num n =
  loop 0 0 num.zero (num.add_int (num.mul_int (num.div_int n 2) 2) 1)
  where rec loop nprime ntwin prev_n n =
    let is_prime =
      loop num.two 1 where rec loop d dd =
        let q = num.div n d in
        if num.lt q d then True
        else if num.eq_int (num.mod_ n d) 0 then False
        else
          let ndd = if num.le_int d 3 || dd = 4 then 2 else 4 in
          loop (num.add_int d dd) ndd
    in
    (*
    if is_prime then do { printf " %d" n; flush stdout; } else ();
    *)
    if is_prime then
      let nprime = nprime + 1 in
      let ntwin =
        if num.eq_int (num.sub n prev_n) 2 then
          (*
          let _ = do { printf " %s-%s" (Bnum.to_string num prev_n) (Bnum.to_string num n); flush stdout; } in
          *)
          let ntwin = ntwin + 1 in
          ntwin + 1
        else ntwin
      in
      loop nprime ntwin n (num.add_int n 2)
    else loop nprime ntwin prev_n (num.add_int n 2)
;

value exists_divisible_by_small num m list =
  loop m (num.of_int 5) 2 list where rec loop cnt d dd =
    fun
    [ [n :: nl] ->
        if num.le n d then loop cnt d dd nl
        else if num.eq_int (num.mod_ n d) 0 then True
        else loop cnt d dd nl
    | [] ->
        if cnt < 0 then False
        else
          loop (cnt - 1) (num.add_int d dd) (if dd = 2 then 4 else 2) list ]
;

value print_list_sdl sdl = do {
  printf "%d" (fst (List.hd sdl));
  if snd (List.hd sdl) = 1 then () else printf "^%d" (snd (List.hd sdl));
  List.iter
    (fun (sd, e) -> do {
       printf " %d" sd;
       if e = 1 then () else printf "^%d" e;
     })
    (List.tl sdl)
};

value apply_factor_function num factor_fun n =
  let (q, sdl) =
    if test_small.val then
      loop [] n where rec loop rev_dl n =
        match Bnum.small_divisor num 2000000 n with
        [ Some d ->
            let rev_dl =
              match rev_dl with
              [ [(d1, e1) :: rdl] ->
                  if d = d1 then [(d1, e1+1) :: rdl]
                  else [(d, 1) :: rev_dl]
              | [] ->
                  [(d, 1)] ]
            in
            loop rev_dl (num.div_int n d)
        | None ->
            (n, List.rev rev_dl) ]
    else (n, [])
  in
  let (prime_or_one_p, s) =
    let n = q in
    if num.eq_int n 1 then (True, "")
    else if num.lt_int n max_int then (Bnum.is_small_prime (num.to_int n), "")
    else if num.mod_int n 2 = 0 then (False, "")
    else (Fmr.is_probably_prime num verbose.val n, "*")
  in
  if prime_or_one_p then do {
    if sdl <> [] then do {
      printf "%s: {" (Bnum.to_string num n);
      print_list_sdl sdl;
      printf "}";
      if num.eq_int q 1 then ()
      else printf " %s%s" (Bnum.to_string num q) s;
    }
    else
      printf "%s: prime number" (Bnum.to_string num n);
    printf "\n";
    flush stdout;
  }
  else do {
    if verbose.val && sdl <> [] then do {
      printf "small divisors: {";
      print_list_sdl sdl;
      printf "}\n";
      printf "quotient: %s\n" (Bnum.to_string num q);
      printf "\n";
      flush stdout;
    }
    else ();
    match factor_fun num verbose.val q with
    [ Some (d1, d2) -> do {
        if sdl <> [] then do {
          printf "%s: {" (Bnum.to_string num n);
          print_list_sdl sdl;
          printf "}";
        }
        else
          printf "%s:" (Bnum.to_string num n);
        let s d =
          if num.lt_int d 20000 then
            if Bnum.is_small_prime (num.to_int d) then "" else "="
          else if num.mod_int d 2 = 0 then "="
          else if Fmr.is_probably_prime num False d then "*"
          else "="
        in
        printf " %s%s %s%s\n" (Bnum.to_string num d1) (s d1)
          (Bnum.to_string num d2) (s d2);
        flush stdout;
      }
    | None -> assert False ]
  }
;

value factor_rho_pollard_brent_main num =
  apply_factor_function num Rho_pollard.factor_brent
;

value factor_rho_pollard_floyd_main num =
  apply_factor_function num Rho_pollard.factor_floyd
;

value factor_quad_sieve_main num =
  apply_factor_function num Quad_sieve.factor
;

(* test prime with Fibonacci sequence *)

module MapString =
  Map.Make (struct type t = string; value compare = compare; end)
;

value is_fibonacci_prime num n =
  let record =
    let rec_map = ref MapString.empty in
    fun f x ->
      let s = num.to_string x in
      match
        try Some (MapString.find s rec_map.val) with [ Not_found -> None ]
      with
      [ Some r -> r
      | None -> do {
          let r = f x in
          rec_map.val := MapString.add s r rec_map.val;
          r
        } ]
  in
  let rec fib i = record fib0 i
  and fib0 i =
    if num.le_int i 1 then i
    else
      let (q, r) = num.quomod_int i 2 in
      let v =
        let ui_on_2 = fib q in
        if r = 0 then
          let ui_on_2_m_1 = fib (num.sub_int q 1) in
          num.mul ui_on_2 (num.addmul_int ui_on_2 ui_on_2_m_1 2)
        else
          let ui_on_2_p_1 = fib (num.add_int q 1) in
          num.add (num.mul ui_on_2 ui_on_2) (num.mul ui_on_2_p_1 ui_on_2_p_1)
      in
      num.mod_ v n
  in
  let x = num.mod_int n 5 in
  if x = 0 then False
  else
    let leg_symb = if x = 1 || x = 4 then 1 else -1 in
    let v = fib (num.sub_int n leg_symb) in
    num.eq_int v 0
;

module MapInt = Map.Make (struct type t = int; value compare = compare; end);

value float_max_int = float max_int;
value max_float_int = 2. ** 50.;

value add_mod a b c =
  let r = mod_float (float a +. float b) (float c) in
  if r > float_max_int then failwith "overflow" else int_of_float r
;

value mult_mod a b c =
  let x = float a *. float b in
  if x > max_float_int then failwith "overflow"
  else
    let r = mod_float x (float c) in
    if r > float_max_int then failwith "overflow" else int_of_float r
;

value is_small_fibonacci_prime n =
  let record =
    let rec_map = ref MapInt.empty in
    fun f x ->
      match
        try Some (MapInt.find x rec_map.val) with [ Not_found -> None ]
      with
      [ Some r -> r
      | None -> do {
          let r = f x in
          rec_map.val := MapInt.add x r rec_map.val;
          r
        } ]
  in
  let rec fib i = record fib0 i
  and fib0 i =
    if i <= 1 then i
    else
      let (q, r) = (i / 2, i mod 2) in
      let ui_on_2 = fib q in
      let v =
        if r = 0 then
          let ui_on_2_m_1 = fib (q - 1) in
          mult_mod ui_on_2 (add_mod ui_on_2 (mult_mod 2 ui_on_2_m_1 n) n) n
        else
          let ui_on_2_p_1 = fib (q + 1) in
          add_mod (mult_mod ui_on_2 ui_on_2 n)
            (mult_mod ui_on_2_p_1 ui_on_2_p_1 n) n
      in
      if v < 0 then failwith "overflow" else v mod n
  in
  let x = n mod 5 in
  if x = 0 then False
  else
    let leg_symb = if x = 1 || x = 4 then 1 else -1 in
    let v = fib (n - leg_symb) in
    v = 0
;

value test_fibonacci num n = do {
  (**)
  if is_fibonacci_prime num n then
    (*
    if is_small_fibonacci_prime (num.to_int n) then
    *)
    printf "premier ou pseudopremier de Fibonacci\n"
  else printf "composé\n";
  flush stdout
};

value small_factor_list =
  loop [] 2 where rec loop list d n =
    let q = n / d in
    if q < d then List.rev [n :: list]
    else if n mod d = 0 then loop [d :: list] d q
    else loop list (d + 1) n
;

value fibonacci_pseudo_primes num =
  loop 0 0 where rec loop cnt_ps cnt_real n = do {
    let s = sprintf "(%s)" (Bnum.to_string num n) in
    eprintf "%s\r" s;
    flush stderr;
    let (cnt_ps, cnt_real) =
      if is_fibonacci_prime num n then
        if not (Bnum.is_small_prime (num.to_int n)) then do {
          (*
          just_factor_nnl (ref n);
          *)
          let n = num.to_int n in
          let fl = small_factor_list n in
          printf "%d:" n;
          List.iter (fun f -> printf " %d" f) fl;
          (**)
          (*
          printf " (%d / %d = %.5f%%)" (cnt_ps + 1) cnt_real
            (float (cnt_ps + 1) /. float cnt_real *. 100.);
          *)
          match fl with
          [ [f1; f2] ->
              let q = f2 / f1 in
              let r = f2 mod f1 in
              let (q, r) = if f1 - r < r then (q + 1, r - f1) else (q, r) in
              if q < 20 && abs r < 20 then
                printf " - n (%sn%s)" (if q = 1 then "" else string_of_int q)
                  (if r < 0 then string_of_int r else "+" ^ string_of_int r)
              else ()
          | _ -> () ];
          printf "\n";
          flush stdout;
          (cnt_ps + 1, cnt_real)
        }
        else (cnt_ps, cnt_real + 1)
      else (cnt_ps, cnt_real)
    in
    loop cnt_ps cnt_real (num.add_int n 1)
  }
;

value fibonacci_small_pseudo_primes =
  loop 0 0 where rec loop cnt_ps cnt_real n = do {
    let s = sprintf "(%s)" (string_of_int n) in
    eprintf "%s\r" s;
    flush stderr;
    let (cnt_ps, cnt_real) =
      if is_small_fibonacci_prime n then
        if not (Bnum.is_small_prime n) then do {
          let fl = small_factor_list n in
          printf "%d:" n;
          List.iter (fun f -> printf " %d" f) fl;
          (*
          printf " (%d / %d = %.5f%%)" (cnt_ps + 1) cnt_real
            (float (cnt_ps + 1) /. float cnt_real *. 100.);
          *)
          match fl with
          [ [f1; f2] ->
              let q = f2 / f1 in
              let r = f2 mod f1 in
              let (q, r) = if f1 - r < r then (q + 1, r - f1) else (q, r) in
              if q < 20 && abs r < 20 then
                printf " - n (%sn%s)" (if q = 1 then "" else string_of_int q)
                  (if r < 0 then string_of_int r else "+" ^ string_of_int r)
              else ()
          | _ -> () ];
          printf "\n";
          flush stdout;
          (cnt_ps + 1, cnt_real)
        }
        else (cnt_ps, cnt_real + 1)
      else (cnt_ps, cnt_real)
    in
    loop cnt_ps cnt_real (n + 1)
  }
;

value select_N num n =
  let log_n = big_approx_log num n in
  let q = truncate (log_n +. 0.5) in
  loop num.one 2 where rec loop nn d =
    if d > q then nn
    else if Bnum.is_small_prime d then
      let log_d_n = truncate (log_n /. log (float d) +. 0.5) in
      loop (num.mul nn (num.power_int_int d log_d_n)) (d + 1)
    else loop nn (d + 1)
;

value factor_fermat num n = do {
  let nn = select_N num n in
  let a = num.add_int (Bnum.random num (num.sub_int n 3)) 2 in
  let g =
    let g = num.gcd a n in
    if num.eq_int g 1 then
      let v = power_big_int_mod num a nn n in
      num.gcd (num.sub_int v 1) n
    else g
  in
  printf "%s %s\n" (Bnum.to_string num g) (Bnum.to_string num (num.div n g));
  flush stdout
};

type ec_point 'a =
  [ ECzero
  | ECpoint of 'a and 'a ]
;

value rec factor_elliptic_loop num n nn =
  let (aa, bb, a) =
    let x = num.add_int (Bnum.random num (num.sub_int n 1)) 1 in
    let y = num.add_int (Bnum.random num (num.sub_int n 1)) 1 in
    let a = ECpoint x y in
    let aa = Bnum.random num n in
    (*
        let bb =
          num.mod_t
            (num.sub
               (num.sub (num.square y)
                  (num.mul (num.square x) x))
               (num.mul aa x))
            n
        in
    *)
    (aa, num.zero, a)
  in
  let _ =
    if verbose.val then do {
      eprintf "y^2=x^3+%s*x+%s\n" (Bnum.to_short_string num aa) "...";
      flush stderr
    }
    else ()
  in
  let ec_add a1 a2 =
    match (a1, a2) with
    [ (ECzero, _) -> Result a2
    | (_, ECzero) -> Result a1
    | (ECpoint x1 y1, ECpoint x2 y2) ->
        let y1py2 = num.add y1 y2 in
        let g = num.gcd y1py2 n in
        if num.eq_int g 1 then
          let m =
            if not (num.eq x1 x2) then
              match inv_big_int_mod num (num.sub x2 x1) n with
              [ Result inv_diff_mod_n ->
                  Result (Some (num.mul (num.sub y2 y1) inv_diff_mod_n))
              | GCD_found g ->
                  GCD_found g ]
            else if not (num.eq_int y1py2 0) then
              match inv_big_int_mod num (num.mul_int y1 2) n with
              [ Result inv_diff_mod_n ->
                  let r =
                    num.mul (num.add (num.mul_int (num.mul x1 x1) 3) aa)
                      inv_diff_mod_n
                  in
                  Result (Some r)
              | GCD_found g ->
                  GCD_found g ]
            else Result None
          in
          match m with
          [ Result (Some m) ->
              let m = num.mod_ m n in
              let x3 = num.mod_ (num.sub (num.mul m m) (num.add x1 x2)) n in
              let y3 = num.mod_ (num.sub (num.mul m (num.sub x1 x3)) y1) n in
              Result (ECpoint x3 y3)
          | Result None -> Result ECzero
          | GCD_found g -> GCD_found g ]
        else GCD_found g ]
  in
  let ext_mult_big_int n a =
    loop ECzero n a where rec loop res n a =
      if num.eq_int n 0 then Result res
      else
        let (q, r) = num.quomod_int n 2 in
        match ec_add a a with
        [ Result ec_add_a_a ->
            if r = 0 then loop res q ec_add_a_a
            else
              match ec_add res a with
              [ Result ec_add_res_a -> loop ec_add_res_a q ec_add_a_a
              | GCD_found g -> GCD_found g ]
        | GCD_found g -> GCD_found g ]
  in
  match ext_mult_big_int nn a with
  [ GCD_found g -> do {
      printf "%s %s\n" (Bnum.to_string num g) (Bnum.to_string num (num.div n g));
      flush stdout
    }
  | Result _ -> factor_elliptic_loop num n nn ]
;

value factor_elliptic num n =
  let nn = select_N num n in
  factor_elliptic_loop num n nn
;

value h = ref 6;
value a = ref 1;
value b = ref 2;
value c = ref 3;

value divisible_by_small_6 num m n =
  (* pourri, because this "if num.le_int n d then num.eq_int n 2";
     to be checked *)
  loop m 2 0 where rec loop cnt d dd =
    if num.le_int n d then num.eq_int n 2
    else if cnt < 0 then False
    else if num.mod_int n d = 0 then True
    else if d = 2 then loop (cnt - 1) 3 0
    else if d = 3 then loop (cnt - 1) 5 2
    else loop (cnt - 1) (d + dd) (if dd = 2 then 4 else 2)
;

value rec prime_6 num n =
  (* hack, special case because code not sure for option "-6" *)
  if test_small.val && divisible_by_small_6 num 20000 n then False
  else do {
    printf "\n*** %s\n" (Bnum.to_short_string num n);
    flush stdout;
    Fmr.is_probably_prime num (not quiet.val) n ||
      prime_6 num (num.add_int n 2)
  }
;

value six_k num p =
  let c1 = h.val * a.val in
  let c2 = h.val * b.val in
  let c3 = h.val * c.val in
  loop p where rec loop k =
    let p1 = num.add_int (num.mul_int k c1) 1 in
    let p2 = num.add_int (num.mul_int k c2) 1 in
    let p3 = num.add_int (num.mul_int k c3) 1 in
    if exists_divisible_by_small num 2000 [p1; p2; p3] then
      let _ =
        if verbose.val then do {
          eprintf "(1) k = %s\r" (Bnum.to_short_string num k);
          flush stderr
        }
        else ()
      in
      loop (num.add_int k 1)
    else
      let _ =
        if verbose.val then do {
          eprintf "(2) k = %s\n" (Bnum.to_short_string num k);
          flush stderr
        }
        else ()
      in
      if prime_6 num p3 && prime_6 num p2 && prime_6 num p1 then do {
        printf "\nTrouvé\n";
        printf "k=%s\n" (Bnum.to_string num k);
        printf "nombre de Carmichael (%dk+1)(%dk+1)(%dk+1) =\n" c1 c2 c3;
        printf "%s\n" (Bnum.to_string num (num.mul (num.mul p1 p2) p3));
        flush stdout
      }
      else loop (num.add_int k 1)
;

value power_modulo num pl = do {
  let r =
    power_big_int_mod num (List.hd (List.tl (List.tl pl)))
      (List.hd (List.tl pl)) (List.hd pl)
  in
  printf "%s\n" (Bnum.to_string num r);
  flush stdout
};

value num = Bnum.mpz;

value main () =
  let _ = Random.self_init () in
  let n = ref [] in
  let act = ref "" in
  let usage =
    "\
usage: factor [option] <expr>
  sans option, factorise <expr> par essais;
  opérateurs
       + - * /
       ^ (puissance)
       | (ou) & (et) : (ou exclusif)
       < (décalage gauche)
       ! factorielle
       # primorielle
  si nombres préfixés par \"b\", lus en binaire\
"
  in
  let speclist =
    [("-c", Arg.Unit (fun () -> act.val := "c"),
      "cherche les (petits) nombres de Carmichael");
     ("-ec", Arg.Unit (fun () -> act.val := "ec"),
      "factorise par courbes elliptiques");
     ("-fb", Arg.Unit (fun () -> act.val := "fb"),
      "test primalité Fibonacci");
     ("-fbp", Arg.Unit (fun () -> act.val := "fbp"),
      "pseudo-premiers de Fibonacci");
     ("-ft", Arg.Unit (fun () -> act.val := "ft"),
      "factorise par méthode théorème de Fermat");
     ("-j", Arg.Unit (fun () -> act.val := "j"),
      "nombre de nombres premiers / nombre de nombres premiers jumeaux");
     ("-mr", Arg.Unit (fun () -> act.val := "mr"),
      "fait un test Fermat Miller-Rabin");
     ("-phime", Arg.Set test_phime, "produit des nombres premiers bidon");
     ("-pi", Arg.Unit (fun () -> act.val := "pi"),
      "compute pi with n decimals");
     ("-pwm", Arg.Unit (fun () -> act.val := "pwm"), "puissance modulo");
     ("-qs", Arg.Unit (fun () -> act.val := "qs"),
      "factorize by quadratic sieve");
     ("-qs_c", Arg.Set Quad_sieve.qs_search_cycles,
      "add cycles searching (quadratic sieve)");
     ("-rhf", Arg.Unit (fun s -> act.val := "rhf"),
      "factorise par méthode rho de Pollard (algo Floyd)");
     ("-rhb", Arg.Unit (fun s -> act.val := "rhb"),
      "factorise par méthode rho de Pollard (algo Brent)");
     ("-s", Arg.Unit (fun () -> act.val := "s"),
      "cherche premier nombre premier supérieur");
     ("-sy", Arg.Unit (fun () -> act.val := "sy"),
      "Syracuse sequence");
     ("-ob", Arg.Int (fun n -> Bnum.out_base.val := max 2 n),
      "<base> definit la base d'affichage");
     ("-v", Arg.Set verbose, "tracer les calculs");
     ("-q", Arg.Set quiet, "ne pas tracer les calculs");
     ("-6", Arg.Unit (fun () -> act.val := "6"),
      sprintf
        "cherche les nombres de Carmichael de la forme (%dk+1)(%dk+1)(%dk+1)"
        (h.val * a.val) (h.val * b.val) (h.val * c.val));
     ("-npt", Arg.Clear test_small,
      "no prior test for small divisors (options -s, -6, -qs)");
     ("-", Arg.String (fun s -> n.val := [s :: n.val]), "")]
  in
  try do {
    Arg.parse speclist (fun s -> n.val := [s :: n.val]) usage;
    if n.val = [] then raise Exit else ();
    let pl =
      List.map (fun s -> Bnum.of_expr num (Istream.of_string s)) n.val
    in
    match act.val with
    [ "c" -> carmichael num (List.hd pl)
    | "ec" -> factor_elliptic num (List.hd pl)
    | "fb" -> test_fibonacci num (List.hd pl)
    | "fbp" -> fibonacci_small_pseudo_primes (num.to_int (List.hd pl))
    | "ft" -> factor_fermat num (List.hd pl)
    | "mr" -> test_fermat_miller_rabin num pl
    | "pi" -> compute_pi num (List.hd pl)
    | "pwm" -> power_modulo num pl
    | "rhf" -> factor_rho_pollard_floyd_main num (List.hd pl)
    | "rhb" -> factor_rho_pollard_brent_main num (List.hd pl)
    | "qs" -> factor_quad_sieve_main num (List.hd pl)
    | "s" -> find_prime num (List.hd pl)
    | "sy" -> syracuse_sequence num (List.hd pl)
    | "j" -> prime_on_twin num (List.hd pl)
    | "6" -> six_k num (List.hd pl)
    | "" -> factor num (List.hd pl)
    | _ -> assert False ]
  }
  with
  [ Exit -> Arg.usage speclist usage ]
;

main ();

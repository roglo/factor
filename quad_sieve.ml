(* $Id: quad_sieve.ml,v 1.5 2017/12/28 10:59:45 deraugla Exp $ *)

open Printf;
open Bnum_def;

value qs_search_cycles = ref False;
value b_limit = ref max_int;

type arr_int 'a 'b =
  { create : int -> 'a;
    clear : 'a -> unit;
    length : 'a -> int;
    get : 'a -> int -> int;
    max_length : 'b;
    sieve : 'a -> int -> int -> int -> unit }
;

value arr_int_1 num =
  {create len = Bytes.make len (Char.chr 0);
   clear a = Bytes.fill a 0 (Bytes.length a) (Char.chr 0);
   length = Bytes.length;
   get a i = Char.code (Bytes.get a i);
   max_length = num.of_int Sys.max_string_length;
   sieve =
     sieve where rec sieve a p v i =
       if i >= Bytes.length a then ()
       else do {
         Bytes.unsafe_set a i
           (Char.unsafe_chr (Char.code (Bytes.unsafe_get a i) + v));
         sieve a p v (i + p)
       }}
;

value arr_int_2 num =
  {create len = Array.make len 0;
   clear a = Array.fill a 0 (Array.length a) 0;
   length = Array.length;
   get = Array.get;
   max_length = num.of_int Sys.max_array_length;
   sieve =
     sieve where rec sieve a p v i =
       if i >= Array.length a then ()
       else do {
         Array.unsafe_set a i (Array.unsafe_get a i + v);
         sieve a p v (i + p)
       }}
;

value arr_int_3 num =
  let msl = Sys.max_string_length in
  {create len =
      let alen = (len + msl - 1) / msl in
      Array.init alen
        (fun i ->
           Bytes.make (if i = alen - 1 then len - (alen - 1) * msl else msl)
             (Char.chr 0))
    ;
   clear arr =
      Array.iter (fun a -> Bytes.fill a 0 (Bytes.length a) (Char.chr 0)) arr
    ;
   length arr = Array.fold_left (fun len a -> len + Bytes.length a) 0 arr;
   get arr i = Char.code (Bytes.get arr.(i / msl) (i mod msl));
   max_length = num.of_int (3 * msl);
   sieve =
     let rec sieve_s s p v i =
       if i >= Bytes.length s then i - Bytes.length s
       else do {
         Bytes.unsafe_set s i
           (Char.chr (Char.code (Bytes.unsafe_get s i) + v));
         sieve_s s p v (i + p)
       }
     in
     sieve where rec sieve arr p v i =
       loop i 0 where rec loop i j =
         if j = Array.length arr then ()
         else
           let i = sieve_s (Array.unsafe_get arr j) p v i in
           loop i (j + 1)}
;

value arr_int = arr_int_1;

value big_approx_log num n = float (num.size_in_base n 2 - 1) *. log 2.;

value string_of_big num n = num.to_string n;

value is_small_prime n =
  loop 2 1 where rec loop d dd =
    if n / d < d then True
    else if n mod d = 0 then False
    else loop (d + dd) (if d <= 3 || dd = 4 then 2 else 4)
;

value jacobi m n =
  if n mod 2 = 0 then failwith "jacobi on even number"
  else
    loop 1 (m mod n) n where rec loop r m n =
      if m = 1 then r
      else if m = 0 then 0
      else if m mod 2 = 0 then
        let v = n mod 8 in
        let r = if v = 1 || v = 7 then r else -r in
        loop r (m / 2) n
      else
        let r = if m mod 4 = 3 && n mod 4 = 3 then -r else r in
        loop r (n mod m) m
;

value is_a_square_mod x p = jacobi x p = 1;

value make_prime_base num n bb =
  (* { p<B, p∈ℙ | ∃k n≡k²[p] } *)
  loop [2] 1 3 where rec loop prime_base bcnt p =
    if bcnt >= bb then List.rev prime_base
    else if is_small_prime p then
      let v = num.mod_int n p in
      if is_a_square_mod v p then loop [p :: prime_base] (bcnt + 1) (p + 2)
      else loop prime_base bcnt (p + 2)
    else loop prime_base bcnt (p + 2)
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

type sieve_data 'a =
  { _X : (int * int) -> 'a;
    _Q : (int * int) -> 'a;
    bb : int;
    sa : int;
    min_last_p : mutable int;
    last_p : mutable list int;
    computed_for_nothing : mutable int;
    count_cycles : mutable int;
    used_cycles : mutable list int;
    extra_factors_ht :
      Hashtbl.t int ((int * int) * list (int * int) * ref int) }
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

value init_a num verbose sd arr bcnt a pb i = do {
  let arr_int = arr_int num in
  let i = (i, 0) in
  let x = sd._X i in
  let y = sd._Q i in
  arr_int.clear arr;
  bcnt.val := 0;
  if verbose then do { eprintf "\n"; flush stderr } else ();
  List.iter
    (fun (p, ndigits_p) -> do {
       if verbose then do {
         incr bcnt;
         eprintf "bcnt=%d p=%d.\027[K" bcnt.val p;
         flush stderr
       }
       else ();
       let r =
         let mod_p v = num.mod_int v p in
         let two_a = 2 * a mod p in
         let a_plus_two_x = mod_p (num.add_int (num.mul_int x 2) a) in
         match inv_mod num a p with
         [ Result inv_a_mod_p ->
             let j = mod_p (num.mul_int x inv_a_mod_p) in
             loop 0 (mod_p y) where rec loop i ui =
               if i = p then []
               else if ui = 0 then
                 if p = 2 then [i] else [p - (i + 2 * j) mod p; i]
               else
                 let uip1 = (ui + i * two_a + a_plus_two_x) mod p in
                 loop (i + 1) uip1
         | GCD_found g -> failwith "gcd found" ]
       in
       if verbose then do { eprintf ".\r"; flush stderr } else ();
       List.iter (arr_int.sieve arr p ndigits_p) r
     })
    pb;
  if verbose then do { eprintf "\n\n"; flush stderr } else ()
};

value gen_geta num verbose dim prime_base sd a =
  let arr_int = arr_int num in
  let arr = arr_int.create dim in
  let pb =
    List.map
      (fun p ->
         let ndigits_p = truncate (log (float p) /. log 2. +. 0.5) in
         (p, ndigits_p))
      prime_base
  in
  let pb =
    loop pb where rec loop =
      fun
      [ [(x, _) :: l] -> if x <= 7 then loop l else l
      | [] -> [] ]
  in
  let bcnt = ref 0 in
  let ia = ref (-1) in
  fun (i, j) -> do {
    if i = ia.val then ()
    else do {
      ia.val := i;
      init_a num verbose sd arr bcnt a pb i
    };
    arr_int.get arr j
  }
;

value nbits_big_int num n = num.size_in_base n 2;

type factorization =
  [ Factors of list (int * int)
  | MissingPrime of list (int * int) and int
  | NotFactorizable ]
;

value factorize_with_prime_base num nn prime_base =
  let rec loop fl n =
    fun
    [ [] ->
        if num.eq_int n 1 then
(*
let _ = do { eeprintf "%s:" (num.to_string nn); List.iter (fun (p, e) -> eeprintf " %d%s" p (if e = 1 then "" else seprintf "^%d" e)) (List.rev fl); eeprintf "\n"; flush stderr } in
*)
          Factors (List.rev fl)
        else if num.lt_int n max_int then
          MissingPrime (List.rev fl) (num.to_int n)
        else NotFactorizable
    | [d :: dl] as gdl ->
        let (q, r) = num.quomod_int n d in
        if num.eq_int q 0 then loop fl n []
        else if r = 0 then
          let fl =
            match fl with
            [ [(pd, e) :: fl] when d = pd -> [(d, e + 1) :: fl]
            | _ -> [(d, 1) :: fl] ]
          in
          loop fl q gdl
        else
          loop fl n dl ]
  in
  if num.lt_int nn 0 then
    loop [(-1, 1)] (num.neg nn) prime_base
  else
    loop [] nn prime_base
;

value compare_index = (compare : (int * int) -> _);

value factor_if_possible num verbose sd nn i prime_base =
  match factorize_with_prime_base num nn prime_base with
  [ Factors fl -> Some (fl, [i])
  | MissingPrime fl n ->
      if not qs_search_cycles.val then None
      else
        match
          try Some (Hashtbl.find sd.extra_factors_ht n) with
          [ Not_found -> None ]
        with
        [ Some (i2, fl2, cnt) -> do {
            if verbose then do {
              sd.count_cycles := sd.count_cycles + 1;
              eprintf "\n*** %d yeah! found cycle %d%s\n" sd.count_cycles n
                (if cnt.val = 0 then ""
                 else sprintf " (%d times)" (cnt.val + 1));
              flush stderr;
            }
            else ();
            Hashtbl.replace sd.extra_factors_ht n (i, fl, cnt);
            if cnt.val = 0 then
              sd.computed_for_nothing := sd.computed_for_nothing - 1
            else ();
            incr cnt;
            let fl =
              loop [] fl fl2 where rec loop rev_fl gfl1 gfl2 =
                match (gfl1, gfl2) with
                [ ([(f1, e1) :: fl1], [(f2, e2) :: fl2]) ->
                    if f1 < f2 then loop [(f1, e1) :: rev_fl] fl1 gfl2
                    else if f1 > f2 then loop [(f2, e2) :: rev_fl] gfl1 fl2
                    else loop [(f1, e1+e2) :: rev_fl] fl1 fl2
                | (gfl1, []) -> List.rev (List.rev_append gfl1 rev_fl)
                | ([], gfl2) -> List.rev (List.rev_append gfl2 rev_fl) ]
            in
            Some (fl, if compare_index i i2 < 0 then [i2; i] else [i; i2])
          }
        | None -> do {
            Hashtbl.add sd.extra_factors_ht n (i, fl, ref 0);
            None
          } ]
  | NotFactorizable ->
      None ]
;

value rec list_up_extract x =
  fun
  [ [] -> None
  | [(a, b) :: l] ->
      if x = a then Some (b, l)
      else if x < a then None
      else list_up_extract x l ]
;

value rec merge_down_lists compare gpl1 gpl2 =
  match (gpl1, gpl2) with
  [ ([p1 :: pl1], [p2 :: pl2]) ->
      let c = compare p1 p2 in
      if c > 0 then [p1 :: merge_down_lists compare pl1 gpl2]
      else if c < 0 then [p2 :: merge_down_lists compare gpl1 pl2]
      else merge_down_lists compare pl1 pl2
  | ([], pl2) -> pl2
  | (pl1, []) -> pl1 ]
;

value substitute_primes verbose sd prime_base subst ii fl = do {
  let ps1 = Array.make (sd.bb + 1) False in
  let (rev_sol1, _, _) =
    List.fold_left
      (fun (sol1, prime_base, i) (f, e) ->
         let (prime_base, i) =
           loop i prime_base where rec loop i =
             fun
             [ [p :: pl] -> if f = p then (pl, i) else loop (i + 1) pl
             | [] -> assert False ]
         in
         let sol1 =
           if e land 1 = 1 then do {
             ps1.(i) := True;
             [f :: sol1]
           }
           else sol1
         in
         (sol1, prime_base, i + 1))
      ([], [-1 :: prime_base], 0) fl
  in
  if verbose then do {
    eprintf "\n  [";
    let sep = ref "" in
    List.iter (fun f -> do { eprintf "%s%d" sep.val f; sep.val := "," })
      (List.rev rev_sol1);
    eprintf "]\n";
    flush stderr
  }
  else ();
  (* apply the substitutions to the new factor list *)
  let (il1, _) =
    List.fold_right
      (fun p (il1, subst) ->
         match list_up_extract p subst with
         [ Some ((ps, il), subst) -> do {
             for i = 0 to sd.bb do {
               if ps.(i) then ps1.(i) := not ps1.(i) else ();
             };
             let il1 = merge_down_lists compare_index il1 il in
             (il1, subst)
           }
         | None -> (il1, subst) ])
      rev_sol1 (ii, subst)
  in
  if verbose then do {
    eprintf "  [";
    if ps1.(0) then eprintf "-1" else ();
    let sep = if ps1.(0) then "," else "" in
    loop sep 1 prime_base where rec loop sep i =
      fun
      [ [p :: pl] -> do {
          if ps1.(i) then eprintf "%s%d" sep p else ();
          let sep = if ps1.(i) then "," else sep in
          loop sep (i + 1) pl
        }
      | [] -> () ];
    eprintf "]\n";
    flush stderr
  }
  else ();
  (ps1, il1)
};

value update_substitution verbose sd subst ps1 il1 p1 i1 = do {
  if verbose then do {
    sd.min_last_p := min sd.min_last_p p1;
    sd.last_p := [p1 :: sd.last_p];
    if List.length sd.last_p > 20 then
      sd.last_p := List.rev (List.tl (List.rev sd.last_p))
    else ();
    let x = List.fold_left min 100000 sd.last_p in
    eprintf "  min_last_p %d %d\n" sd.min_last_p x;
    flush stderr
  }
  else ();
  let subst =
    List.map
      (fun (p, (ps, il)) ->
         if ps.(i1) then
           let _ =
             for i = 0 to sd.bb do {
               if ps1.(i) then ps.(i) := not ps.(i) else ();
             }
           in
           let il = merge_down_lists compare_index il il1 in
           (p, (ps, il))
         else (p, (ps, il)))
      subst
  in
  (* add the new substitution *)
  List.merge (fun x y -> compare (fst x) (fst y)) [(p1, (ps1, il1))]
    subst
};

value quad_sieve_sol num sd n blist il1 =
  let (x, y_fact_list, y0) =
    List.fold_left
      (fun (prod_x, y_fact_list, y0) i ->
         let x = sd._X i in
         let prod_x = num.mod_ (num.mul prod_x x) n in
(**)
         match
           match factorize_with_prime_base num (sd._Q i) blist with
           [ MissingPrime fl f -> do {
               sd.used_cycles := [f :: sd.used_cycles];
               Factors (List.merge compare [(f, 1)] fl)
             }
           | x -> x ]
         with
(*
         match Factors (factorize_brute_force (sd._Q i)) with
*)
         [ Factors fl ->
             let fl =
               loop fl y_fact_list where rec loop gfl1 gfl2 =
                 match (gfl1, gfl2) with
                 [ ([(f1, e1) :: fl1], [(f2, e2) :: fl2]) ->
                     if f1 < f2 then [(f1, e1) :: loop fl1 gfl2]
                     else if f1 > f2 then [(f2, e2) :: loop gfl1 fl2]
                     else [(f1, e1 + e2) :: loop fl1 fl2]
                 | ([], _) -> gfl2
                 | (_, []) -> gfl1 ]
             in
             (prod_x, fl, num.mod_ (num.mul_int y0 sd.sa) n)
         | MissingPrime _ _ | NotFactorizable -> assert False ])
      (num.one, [], num.one) il1
  in
(*
let _ = do { eprintf "\nresult:\n\n"; List.iter (fun (d, e) -> eprintf "\t%d^%d" d e) y_fact_list; eprintf "\n"; flush stderr } in
*)
  let y =
    List.fold_left
      (fun prod_y (f, e) -> do {
(*
let _ = if e mod 2 = 0 then () else do { eprintf "%d^%d\n" f e; flush stderr } in
*)
         assert (e mod 2 = 0);
         let e = e / 2 in
         loop prod_y e where rec loop prod_y e =
           if e = 0 then prod_y
           else loop (num.mod_ (num.mul_int prod_y f) n) (e - 1)
       })
      y0 y_fact_list
  in
  (x, y)
;

value test_quad_sieve_sol num verbose sd prime_base n il1 = do {
  sd.used_cycles := [];
  let (x, y) = quad_sieve_sol num sd n prime_base il1 in
  let g = num.gcd n (num.add x y) in
  if verbose then do {
    eprintf "n %s\n" (string_of_big num n);
    eprintf "x %s\n" (string_of_big num x);
    eprintf "y %s\n" (string_of_big num y);
    eprintf "gcd(n,x+y) %s\n" (string_of_big num g);
    eprintf "gcd(n,x-y) %s\n" (string_of_big num (num.gcd n (num.sub x y)));
  }
  else ();
  if verbose then eprintf "\n" else ();
  if num.eq_int g 1 || num.eq g n then None
  else do {
    if verbose then do {
      if sd.used_cycles <> [] then do {
        eprintf "used cycles:";
        loop (List.sort compare sd.used_cycles) where rec loop =
          fun
          [ [x; y :: l] -> do {
              assert (x = y);
              eprintf " %d" x;
              loop l
            }
          | [] -> ()
          | [_] -> assert False ];
        eprintf "\n";
      }
      else ();
      eprintf "computed for nothing: %d\n" sd.computed_for_nothing;
    }
    else ();
    let q = num.div n g in
    let (d1, d2) = if num.lt q g then (q, g) else (g, q) in
    Some (d1, d2)
  }
};

value factor_aux num verbose n bb = do {
  let mm = num.power_int (num.of_int bb) 3 in
  if verbose then do {
    eprintf "B=%d\nM=%s\n" bb (string_of_big num mm);
    flush stderr
  }
  else ();
  let arr_int = arr_int num in
  let prime_base = make_prime_base num n bb in
  let bdim = num.min arr_int.max_length mm in
  let dim = num.to_int bdim in
  let zero_index = (0, 0) in
  let succ_index (i, j) =
    let j = j + 1 in
    if j = dim then (i + 1, 0) else (i, j)
  in
  let big_int_of_index (i, j) =
    let r = num.of_int j in
    if i = 0 then r
    else if i land 1 = 0 then num.addmul_int r bdim (i lsr 1)
    else num.submul_int r bdim ((i + 1) lsr 1)
  in
  let sa = 1 in
  let a = sa * sa in
  let b = num.add_int (num.sqrt n) 1 in
  let two_b = num.mul_int b 2 in
  let c =
    (* (b²-n)/a mod n *)
    let inv_a_mod_n =
      match inv_big_int_mod num (num.of_int a) n with
      [ Result r -> r
      | GCD_found g -> failwith "gcd found" ]
    in
    num.mod_ (num.mul (num.sub (num.mul b b) n) inv_a_mod_n) n
  in
  assert
    (num.eq_int (num.mod_ (num.sub (num.mul b b) (num.mul_int c a)) n)
       0);
  let sd =
    {_X i = num.addmul_int b (big_int_of_index i) a;
     _Q i =
       (* ai²+2bi+c *)
       let bi = big_int_of_index i in
       num.addmul c (num.addmul_int two_b bi a) bi;
     bb = bb; sa = sa;
     min_last_p = List.hd (List.rev prime_base); last_p = [];
     computed_for_nothing = 0; count_cycles = 0; used_cycles = [];
     extra_factors_ht = Hashtbl.create 41959}
  in
  let geta = gen_geta num verbose dim prime_base sd a in
  let nbits1 = nbits_big_int num (sd._Q zero_index) in
  let nbits2 =
    let imm =
      let (q, r) = num.quomod mm bdim in
      (num.to_int q, num.to_int r)
    in
    nbits_big_int num (sd._Q imm)
  in
  if verbose then do {
    eprintf "nbits1 %d nbits2 %d\n" nbits1 nbits2;
    flush stderr
  }
  else ();
  let nbits_biggest_prime =
    (* not sure it is correct to compute that *)
    let bigger_prime = List.hd (List.rev prime_base) in
    truncate (log (float bigger_prime) /. log 2.)
  in
  loop prime_base [] 0 zero_index where rec loop prime_base subst cnt i =
    if geta i < nbits1 - 5 then
      (* approximation to avoid computing y *)
      loop prime_base subst cnt (succ_index i)
    else
      let y = sd._Q i in
      let nbits_y = nbits_big_int num y in
      let gi = geta i in
      if nbits_y - gi > nbits_biggest_prime + 1 then
        loop prime_base subst cnt (succ_index i)
      else do {
        if verbose then do {
          eprintf "%d (B=%d): i=(%d,%d) y=%s nbits_y=%d restant=%d\027[K\r"
            cnt sd.bb (fst i) (snd i) (string_of_big num y) nbits_y
            (nbits_y - gi);
          flush stderr
        }
        else ();
        match factor_if_possible num verbose sd y i prime_base with
        [ Some (fl, il) ->
            let (ps1, il1) =
              substitute_primes verbose sd prime_base subst il fl
            in
            let i1p1 =
              loop sd.bb where rec loop i =
                if i < 0 then None
                else if ps1.(i) then Some (i, List.nth [-1 :: prime_base] i)
                else loop (i - 1)
            in
            match i1p1 with
            [ Some (i1, p1) ->
                let subst =
                  update_substitution verbose sd subst ps1 il1 p1 i1
                in
                loop prime_base subst (cnt + 1) (succ_index i)
            | None ->
                match test_quad_sieve_sol num verbose sd prime_base n il1 with
                [ Some (d1, d2) -> Some (d1, d2)
                | None -> loop prime_base subst (cnt + 1) (succ_index i) ] ]
        | None ->
            let _ = sd.computed_for_nothing := sd.computed_for_nothing + 1 in
            loop prime_base subst cnt (succ_index i) ]
      }
};

value factor num verbose n = do {
  let lnn = big_approx_log num n in
  let v = exp (sqrt (lnn *. log lnn)) ** (sqrt 2. /. 4.) in
  let v = min 1000000.0 v in
  let bb = truncate (v +. 0.5) in
  let bb = max bb 10 in
  if bb >= b_limit.val then None
  else factor_aux num verbose n bb
};

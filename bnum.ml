open Bnum_def;

value oabs = abs;
value ocompare = compare;
value omin = min;

value int =
  let not_impl x = failwith ("int." ^ x) in
  let power_mod a b p =
    let mul_mod x y p = x * y mod p in
    loop 1 b a where rec loop res n a =
      if n = 0 then res
      else
        let (q, r) = (n / 2, n mod 2) in
        if r = 0 then loop res q (mul_mod a a p)
        else loop (mul_mod res a p) q (mul_mod a a p)
  in
  {zero = 0;
   one = 1;
   two = 2;
   of_int i = i;
   to_int x = x;
   of_float f = truncate (f +. 0.5);
   eq = \=;
   eq_int = \=;
   gt = \>;
   gt_int = \>;
   ge = \>=;
   ge_int = \>=;
   lt = \<;
   lt_int = \<;
   le = \<=;
   le_int = \<=;
   compare = ocompare;
   abs = oabs;
   neg x = -x;
   add = \+;
   add_int = \+;
   sub = \-;
   sub_int = \-;
   mul = \*;
   mul_int = \*;
   div = \/;
   div_int = \/;
   mod_ = \mod;
   mod_int = \mod;
   quomod x i = (x / i, x mod i);
   quomod_int x i = (x / i, x mod i);
   addmul a b c = a + b * c;
   addmul_int a b c = a + b * c;
   submul a b c = a - b * c;
   submul_int a b c = a - b * c;
   power x y = not_impl "power";
   power_int a b = not_impl "power_int";
   power_int_int a b = not_impl "power_int_int";
   power_mod = power_mod;
   power_mod_int = power_mod;
   log x = log (float x);
   land_ = \land;
   lor_ = \lor;
   lxor_ = \lxor;
   sqrt x = not_impl "sqrt";
   min = omin;
   gcd = gcd where rec gcd p q = if q = 0 then p else gcd q (p mod q);
   randstate_self_init () = let () = Random.self_init () in {rs () = ()};
   randstate_init x = let () = Random.init x in {rs () = ()};
   size_in_base x b = not_impl "size_in_base";
   of_string = int_of_string;
   to_string = string_of_int}
;

value rec big =
  let not_impl x = failwith ("big." ^ x) in
  let lop_big_int comp lop b1 b2 = do {
    let n1 = Big_int.nat_of_big_int b1 in
    let n2 = Big_int.nat_of_big_int b2 in
    let len1 = Nat.length_nat n1 in
    let len2 = Nat.length_nat n2 in
    let (n1, n2, len1, len2) =
      if comp len1 len2 then (n1, n2, len1, len2) else (n2, n1, len2, len1)
    in
    let r = Nat.create_nat len1 in
    Nat.blit_nat r 0 n1 0 len1;
    for i = 0 to omin len1 len2 - 1 do { lop r i n2 i };
    Big_int.big_int_of_nat r
  }
  in
  let power_mod a b p =
    let mul_mod x y p = big.mod_ (big.mul x y) p in
    loop big.one b a where rec loop res n a =
      if big.eq_int n 0 then res
      else
        let (q, r) = big.quomod_int n 2 in
        if r = 0 then loop res q (mul_mod a a p)
        else loop (mul_mod res a p) q (mul_mod a a p)
  in
  {zero = Big_int.zero_big_int;
   one = Big_int.unit_big_int;
   two = Big_int.big_int_of_int 2;
   of_int = Big_int.big_int_of_int;
   to_int = Big_int.int_of_big_int;
   of_float _ = not_impl "of_float";
   eq = Big_int.eq_big_int;
   eq_int x i = Big_int.eq_big_int x (big.of_int i);
   le = Big_int.le_big_int;
   le_int x i = Big_int.le_big_int x (big.of_int i);
   lt = Big_int.lt_big_int;
   lt_int x i = Big_int.lt_big_int x (big.of_int i);
   ge = Big_int.ge_big_int;
   ge_int x i = Big_int.ge_big_int x (big.of_int i); 
   gt = Big_int.gt_big_int;
   gt_int x i = Big_int.gt_big_int x (big.of_int i);
   compare = Big_int.compare_big_int;
   abs = Big_int.abs_big_int;
   neg = Big_int.minus_big_int;
   add = Big_int.add_big_int;
   add_int x i = Big_int.add_int_big_int i x;
   sub = Big_int.sub_big_int;
   sub_int x i = Big_int.add_int_big_int (-i) x;
   mul = Big_int.mult_big_int;
   mul_int x i = Big_int.mult_int_big_int i x;
   div = Big_int.div_big_int;
   div_int x i = Big_int.div_big_int x (big.of_int i);
   mod_ = Big_int.mod_big_int;
   mod_int x i =
     Big_int.int_of_big_int (Big_int.mod_big_int x (big.of_int i))
   ;
   addmul x y z = Big_int.add_big_int x (Big_int.mult_big_int y z);
   addmul_int x y i = Big_int.add_big_int x (Big_int.mult_int_big_int i y);
   submul x y z = Big_int.sub_big_int x (Big_int.mult_big_int y z);
   submul_int x y i = Big_int.sub_big_int x (Big_int.mult_int_big_int i y);
   sqrt = Big_int.sqrt_big_int;
   size_in_base n b =
     if b = 2 then
       let n = Big_int.nat_of_big_int (Big_int.abs_big_int n) in
       let len = Nat.length_nat n in
       Nat.length_of_digit * len -
         Nat.num_leading_zero_bits_in_digit n (len - 1)
     else
       not_impl "size_in_base"
   ;
   quomod = Big_int.quomod_big_int;
   quomod_int x i =
     let (q, r) = Big_int.quomod_big_int x (big.of_int i) in
     (q, big.to_int r)
   ;
   power = Big_int.power_big_int_positive_big_int;
   power_int = Big_int.power_big_int_positive_int;
   power_int_int = Big_int.power_int_positive_int;
   power_mod = power_mod;
   power_mod_int a i c = power_mod a (Big_int.big_int_of_int i) c;
   log _ = not_impl "log";
   land_ = lop_big_int \< Nat.land_digit_nat;
   lor_ = lop_big_int \> Nat.lor_digit_nat;
   lxor_ = lop_big_int \> Nat.lxor_digit_nat;
   min = Big_int.min_big_int;
   gcd = Big_int.gcd_big_int;
   randstate_self_init () = not_impl "randstate_self_init";
   randstate_init _ = not_impl "randstate_init";
   of_string = Big_int.big_int_of_string;
   to_string = Big_int.string_of_big_int}
;

value rec mpz =
  let not_impl x = failwith ("mpz." ^ x) in
  let div_2exp n k =
    let lim = 53 in
    if k <= lim then Mpz.to_float n /. 2.0 ** float k
    else ldexp (Mpz.to_float (Mpz.div_q_2exp n (k - lim))) (-lim)
  in
  let rs = ref None in
  {zero = Mpz.of_int 0;
   one = Mpz.of_int 1;
   two = Mpz.of_int 2;
   of_int = Mpz.of_int;
   to_int = Mpz.to_int;
   of_float = Mpz.of_float;
   eq x y = Mpz.compare x y = 0;
   eq_int x i = Mpz.compare_si x i = 0;
   gt x y = Mpz.compare x y > 0;
   gt_int x i = Mpz.compare_si x i > 0;
   ge x y = Mpz.compare x y >= 0;
   ge_int x i = Mpz.compare_si x i >= 0;
   lt x y = Mpz.compare x y < 0;
   lt_int x i = Mpz.compare_si x i < 0;
   le x y = Mpz.compare x y <= 0;
   le_int x i = Mpz.compare_si x i <= 0;
   compare = Mpz.compare;
   abs = Mpz.abs;
   neg = Mpz.neg;
   add = Mpz.add;
   add_int = Mpz.add_ui;
   sub = Mpz.sub;
   sub_int = Mpz.sub_ui;
   mul = Mpz.mul;
   mul_int = Mpz.mul_si;
   div = Mpz.div_q;
   div_int = Mpz.div_q_ui;
   mod_ = Mpz.div_r;
   mod_int = Mpz.div_r_ui;
   quomod = Mpz.div_qr;
   quomod_int = Mpz.div_qr_ui;
   addmul = Mpz.addmul;
   addmul_int x y i =
     if i >= 0 then Mpz.addmul_ui x y i
     else not_impl "addmul_int"
   ;
   submul = Mpz.submul;
   submul_int x y i =
     if i >= 0 then Mpz.submul_ui x y i
     else not_impl "submul_int"
   ;
   power x y =
     if Mpz.compare y mpz.zero >= 0 && Mpz.compare y (Mpz.of_int max_int) < 0
     then
       Mpz.pow_ui x (Mpz.to_int y)
     else not_impl "power"
   ;
   power_int x i =
     if Mpz.compare x mpz.zero >= 0 &&
        Mpz.compare x (Mpz.of_int max_int) < 0 && i >= 0
     then
       Mpz.ui_pow_ui (Mpz.to_int x) i
     else not_impl "power_int"
   ;
   power_int_int = Mpz.ui_pow_ui;
   power_mod = Mpz.pow_mod;
   power_mod_int = Mpz.pow_mod_ui;
   log n =
     let k = Mpz.size_in_base n 2 - 1 in
     float k *. log 2.0 +. log (div_2exp n k)
   ;
   land_ = Mpz.l_and;
   lor_ = Mpz.l_ior;
   lxor_ = Mpz.l_xor;
   sqrt = Mpz.sqrt;
   min x y = if Mpz.compare x y < 0 then x else y;
   gcd = Mpz.gcd;
   randstate_self_init () = do {
     Random.self_init ();
     let v = Mpz.randstate_init (Mpz.of_int (Random.int max_int)) in
     {rs () = rs.val := Some v}
   };
   randstate_init x =
     let v = Mpz.randstate_init x in
     {rs () = rs.val := Some v}
   ;
   size_in_base = Mpz.size_in_base;
   of_string s = do {
     if s = "" then failwith "Num_mpz.of_string"
     else
       for i = 0 to String.length s - 1 do {
         match s.[i] with
         [ '0'..'9' -> ()
         | _ -> failwith "Num_mpz.of_string" ];
       };
     Mpz.of_string 10 s
   };
   to_string = Mpz.to_string 10}
;

open Printf;

value lshift_left_big_int num b sh =
  let _ = if sh <> 1 then failwith "lshift_left_big_int not impl" else () in
  num.mul_int b 2
;

value power_big_int_positive_big_int num x1 x2 =
  try num.power x1 x2 with
  [ Failure s ->
      failwith (sprintf "%s: %s %s" s (num.to_string x1) (num.to_string x2)) ]
;

value factorial num =
  factorial where rec factorial n =
    if num.eq_int n 1 then n else num.mul n (factorial (num.sub_int n 1))
;

value is_small_prime n =
  if n < 2 then False
  else
    loop 2 1 where rec loop d dd =
      if n / d < d then True
      else if n mod d = 0 then False
      else loop (d + dd) (if d <= 3 || dd = 4 then 2 else 4)
;

value small_divisor num m n =
  loop m 2 0 where rec loop cnt d dd =
    if cnt < 0 then None
    else if num.mod_int n d = 0 then Some d
    else if d = 2 then loop (cnt - 1) 3 0
    else if d = 3 then loop (cnt - 1) 5 2
    else loop (cnt - 1) (d + dd) (if dd = 2 then 4 else 2)
;

value divisible_by_small num m n = small_divisor num m n <> None;

value primorial num x1 =
  let x = num.to_int x1 in
  primorial 2 num.one where rec primorial p r =
    if p > x then r
    else
      let r = if is_small_prime p then num.mul_int r p else r in
      primorial (p + 1) r
;

value of_expr num strm =
  let rec expr = parser [: x1 = term; a = expr_kont x1 :] -> a
  and expr_kont x1 =
    parser
    [ [: `'+'; x2 = term ? "term expected";
         a = expr_kont (num.add x1 x2) :] ->
        a
    | [: `'-'; x2 = term ? "term expected";
         a = expr_kont (num.sub x1 x2) :] ->
        a
    | [: `'|'; x2 = term ? "term expected";
         a = expr_kont (num.lor_ x1 x2) :] ->
        a
    | [: :] -> x1 ]
  and term = parser [: x1 = factor; a = term_kont x1 :] -> a
  and term_kont x1 =
    parser
    [ [: `'*'; x2 = factor ? "factor expected";
         a = term_kont (num.mul x1 x2) :] ->
        a
    | [: `'/'; x2 = factor ? "factor expected";
         a = term_kont (num.div x1 x2) :] ->
        a
    | [: `'&'; x2 = factor ? "factor expected";
         a = term_kont (num.land_ x1 x2) :] ->
        a
    | [: :] -> x1 ]
  and factor = parser [: x1 = factor_ial; a = factor_kont x1 :] -> a
  and factor_kont x1 =
    parser
    [ [: `'^'; x2 = factor ? "factor expected";
         a = factor_kont (power_big_int_positive_big_int num x1 x2) :] ->
        a
    | [: `':'; x2 = factor_ial ? "atom expected";
         a = factor_kont (num.lxor_ x1 x2) :] ->
        a
    | [: `'<'; x2 = factor_ial ? "atom expected";
         a = factor_kont (lshift_left_big_int num x1 (num.to_int x2)) :] ->
        a
    | [: x2 = exponent;
         a = factor_kont (power_big_int_positive_big_int num x1 x2) :] ->
        a
    | [: :] -> x1 ]
  and exponent =
    parser
    [ [: `'\178' :] -> num.of_int 2
    | [: `'\179' :] -> num.of_int 3
    | [: `'\194'; `'\178' :] -> num.of_int 2
    | [: `'\194'; `'\179' :] -> num.of_int 3 ]
  and factor_ial = parser [: x1 = atom; a = factor_ial_kont x1 :] -> a
  and factor_ial_kont x1 =
    parser
    [ [: `'!'; a = factor_ial_kont (factorial num x1) :] -> a
    | [: `'#'; a = factor_ial_kont (primorial num x1) :] -> a
    | [: :] -> x1 ]
  and atom =
    parser
    [ [: `'b'; a = number 2 binary :] -> a
    | [: `'('; a = expr; `')' ? "')' expected" :] -> a
    | [: `'-'; a = atom :] -> num.neg a
    | [: a = number 10 decimal :] -> a ]
  and number base digit =
    parser [: x = digit; a = number_kont base digit (num.of_int x) :] -> a
  and number_kont base digit x =
    parser
    [ [: c = digit;
         a =
           number_kont base digit
             (num.add_int (num.mul_int x base) c) ! :] ->
        a
    | [: `' ' | '_'; a = number_kont base digit x ! :] -> a
    | [: :] -> x ]
  and binary = parser [: `('0'..'1' as c) :] -> Char.code c - Char.code '0'
  and decimal =
    parser [: `('0'..'9' as c) :] -> Char.code c - Char.code '0'
  in
  let main =
    parser [: x = expr; _ = Istream.empty ? "end of string expected" :] -> x
  in
  try main strm with
  [ Istream.Failure | Istream.Error _ -> failwith "syntax error in expression" ]
;

value rand_aux num =
  loop num.zero where rec loop res x =
    if num.eq_int x 0 then res
    else
      let res = num.mul_int res 2 in
      loop (if Random.bool () then num.add_int res 1 else res)
        (num.div_int x 2)
;

value rec random num a =
  if num.le_int a 0 then invalid_arg "Bnum.random"
  else
    let res = rand_aux num a in
    if num.ge res a then random num a else res
;

value out_base = ref 10;

value digit_of_int n =
  if n < 10 then Char.chr (Char.code '0' + n)
  else if n < 36 then Char.chr (Char.code 'A' + n - 10)
  else if n < 62 then Char.chr (Char.code 'a' + n - 36)
  else '?'
;

value list_iteri f l =
  loop 0 l where rec loop i =
    fun
    [ [x :: l] ->
        let () = f i x in
        loop (i + 1) l
    | [] -> () ]
;

value base_string_of_big num out_base n =
  if out_base = 10 then num.to_string n
  else if num.eq_int n 0 then "0"
  else
    let (n, sign) =
      if num.lt_int n 0 then (num.neg n, "-") else (n, "")
    in
    loop [] n where rec loop digits n =
      if num.eq_int n 0 then do {
        let s = Bytes.create (List.length digits) in
        list_iteri (fun i c -> s.[i] := digit_of_int c) digits;
        sprintf "%s%s%s" sign
          (match out_base with
           [ 2 -> "b"
           | 8 -> "o"
           | 16 -> "x"
           | d -> string_of_int d ^ "#" ])
          (Bytes.to_string s)
      }
      else
        let (n, d) = num.quomod_int n out_base in
        loop [d :: digits] n
;

value to_string num n = base_string_of_big num out_base.val n;

value to_short_string num n =
  let s = to_string num n in
  let len = String.length s in
  if len > 30 then
    String.sub s 0 10 ^ "..." ^ String.sub s (len - 10) 10 ^ " (" ^
    string_of_int (String.length s) ^ " digits)"
  else s
;

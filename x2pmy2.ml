(* $Id: x2pmy2.ml,v 1.4 2009/11/15 01:42:56 deraugla Exp $ *)
(* Congruences modulo n des nombres premiers impairs de la forme x²+m*y²;
   Variation de m
   Variation de n
   Variation de x et y *)

open Printf;

value nb_disp_primes = 100;
value max_congruence = 30;
value max_xplusy = 100;

value rec gcd a b = if b = 0 then a else gcd b (a mod b);

value is_odd_prime n =
  if n = 3 then True
  else if n mod 2 = 0 || n mod 3 = 0 then False
  else
    loop 5 2 where rec loop d dd =
      if n / d < d then True
      else if n mod d = 0 then False
      else loop (d + dd) (6 - dd)
;

module S = Set.Make (struct type t = int; value compare = compare; end);

value x2pmy2_mnxy m n xpy =
  let a = Array.make n 0 in
  loop S.empty xpy 1 where rec loop primes xpy x =
    if xpy = max_xplusy then (a, S.elements primes)
    else
      let y = xpy - x in
      if y < 1 then loop primes (xpy + 1) 1
      else
        let primes =
          if gcd x y = 1 then
            let r = x * x + m * y * y in
            if is_odd_prime r then
              do {
                let i = r mod n in
                a.(i) := a.(i) + 1;
                S.add r primes
              }
            else primes
          else primes
       in
       loop primes xpy (x + 1)
;

value print_result m n a =
  do {
    printf "x²+%sy² [mod %d] ="
      (if m = 1 then "" else string_of_int m) n;
    for i = 0 to n - 1 do {
      if a.(i) > 0 then printf " %d" i else ();
    };
  }
;

value rec x2pmy2_mn m n =
  do {
    let (a, primes) = x2pmy2_mnxy m n 2 in
    if n = 3 then
      do {
        printf "(";
        loop "" nb_disp_primes primes where rec loop sep cnt pl =
          match (cnt, pl) with
          [ (0, _) -> printf "..."
          | (_, [p :: pl]) ->
              do { printf "%s%d" sep p; loop "," (cnt - 1) pl }
          | _ -> () ];
        printf ")\n";
      }
    else ();
    let s =
      List.fold_left (fun s x -> if x > 0 then s + 1 else s)
         0 (List.tl (Array.to_list a))
    in
    if s >= n - 1 then ()
    else
      do {
	print_result m n a;
	printf "\n";
        flush stdout;
      };
    if n < max_congruence then x2pmy2_mn m (n + 1) else ()
  }
;

value rec x2pmy2_m m =
  do {
    x2pmy2_mn m 3;
    x2pmy2_m (m + 1)
  }
;

value x2pmy2 () =
  x2pmy2_m 1
;

value x2pmy2_mn_fixed m n =
  do {
    let (a, primes) = x2pmy2_mnxy m n 2 in
    print_result m n a;
    printf "\n";
    flush stdout
  }
;

value main () =
  match Array.length Sys.argv with
  [ 2 -> x2pmy2_mn (int_of_string Sys.argv.(1)) 3
  | 3 ->
      x2pmy2_mn_fixed (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2))
  | _ -> x2pmy2 () ]
;

main ();

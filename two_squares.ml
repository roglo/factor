(* $Id: two_squares.ml,v 1.11 2009/12/18 09:39:42 deraugla Exp $ *)
(* Fermat's theorem of two squares:
   if p is an odd prime number p congruent to 1 mod 4, there exist integers
   a and b such that a²+b² = p *)

open Printf;

value e2 = Stringexp.e2;

value is_small_prime n =
  loop 2 1 where rec loop d dd =
    if n / d < d then True
    else if n mod d = 0 then False
    else loop (d + dd) (if d <= 3 || dd = 4 then 2 else 4)
;

value a_power_b_mod_c a b c =
  let a = a mod c in
  loop 1 a b where rec loop r a b =
    (* loop constant: result = r * a ^ b *)
    if b = 0 then r
    else
      let r =
        if b land 1 = 1 then
          (* r*a^b = r*a^(2k+1) = r*(a^2)^k*a = (r*a)*(a^2)^k *)
          (r * a) mod c
        else
          (* r*a^b = r*a^(2k) = r*(a^2)^k *)
          r
      in
      loop r ((a * a) mod c) (b lsr 1)
;

value find_two_squares n =
  loop_ab 1 1 where rec loop_ab a b =
    if a = n then None
    else if b > a then loop_ab (a + 1) 1
    else if a*a+b*b = n then Some (a, b)
    else loop_ab a (b + 1)
;

value main () =
  let first_num =
    if Array.length Sys.argv = 2 then int_of_string Sys.argv.(1) else 2
  in
  loop first_num where rec loop p = do {
    loop_ab 0 1 1 where rec loop_ab nb_times a b =
      if a = p then ()
      else if b = a then loop_ab nb_times (a + 1) 1
      else if a*a+b*b = p && is_small_prime p then do {
        let nb_times = nb_times + 1 in
        if nb_times > 1 then printf "%d times\n" nb_times else ();
        printf "%d%s=%d%s+%d%s" p (if is_small_prime p then "" else "*") a e2
          b e2;
        if nb_times = 1 then
          loop 1 where rec loop m =
            if m = p then ()
            else do {
              if (m * m + 1) mod p = 0 then do {
                let k = (m * m + 1) / p in
                printf "\t%d.%d=%d%s+1" k p m e2;
              }
              else ();
              loop (m + 1)
            }
        else ();
        printf "\n";
        flush stdout;
        loop_ab nb_times a (b + 1);
      }
      else loop_ab nb_times a (b + 1);
    loop (p + 1)
  }
;

value rec list_uniq cmp =
  fun
  [ [x :: l] ->
      let l = list_uniq cmp l in
      match l with
      [ [y :: _] -> if cmp x y = 0 then l else [x :: l]
      | [] -> [x] ]
  | [] -> [] ]
;

(*
value main () =
  loop 3 where rec loop m = do {
    let n = m * m + 1 in
    let pl =
      loop [] n 2 where rec loop rev_dl n d =
        let q = n / d in
        if q < d then List.rev [n :: rev_dl]
        else if n mod d = 0 then loop [d :: rev_dl] q d
        else loop rev_dl n (d + 1)
    in
    printf "%d=%d%s+1=" n m e2;
    loop "" pl where rec loop sep =
      fun
      [ [p :: pl] -> do {
          printf "%s%d" sep p;
          assert (p = 2 || p mod 4 = 1);
          loop "." pl
        }
      | [] -> () ];
    printf "\n";
    let pl = list_uniq compare pl in
    List.iter
      (fun n -> do {
         printf "  ";
         printf "%d" n;
         printf "=";
         match find_two_squares n with
         [ Some (a, b) -> printf "%d%s+%d%s" a e2 b e2
         | None -> raise Not_found ];
         printf "\n";
       })
      pl;
    flush stdout;
    loop (m + 1)
  }
;
*)

main ();

(* $Id: order.ml,v 1.3 2009/11/17 13:39:10 deraugla Exp $ *)

open Printf;

value rec gcd a b = if b = 0 then a else gcd b (a mod b);

value phi n =
  loop 0 1 where rec loop r d =
    if d > n then r
    else if gcd n d = 1 then loop (r + 1) (d + 1)
    else loop r (d + 1)
;

value find_order d n =
  loop [] 1 where rec loop ol a =
    if a > n then List.rev ol
    else if gcd a n = 1 then
      let ord =
        loop 1 a where rec loop r an =
          if r > d then r
          else if an = 1 then r
          else loop (r + 1) ((an * a) mod n)
      in
      if ord = d then loop [a :: ol] (a + 1) else loop ol (a + 1)
    else loop ol (a + 1)
;

value main () =
  loop 2 where rec loop n = do {
    let phi_n = phi n in
    if phi_n < n - 1 then ()
    else do {
      printf "phi(%d)=%d\n" n phi_n;
      loop 1 where rec loop d =
        if d > phi_n then ()
        else do {
          if phi_n mod d = 0 then do {
            printf "  %d:" d;
            flush stderr;
            match find_order d n with
            [ [] -> raise Not_found
            | [o :: ol] -> do {
                let len = 1 + List.length ol in
                printf " ord(%d" o;
                if len = 1 then ()
                else do {
                  printf ",%d" (List.hd ol);
                  if len = 2 then ()
                  else do {
                    printf ",%d" (List.hd (List.tl ol));
                    if len = 3 then ()
                    else do {
                      printf ",%d" (List.hd (List.tl (List.tl ol)));
                      if len = 4 then () else printf ",...";
                    }
                  }
                };
                printf ") = %d" d;
                if len >= 2 then do {
                  assert (len = phi d);
                  printf " (%d solution(s) = phi(%d))" len d;
                }
                else ();
                printf "\n";
                flush stdout;
              } ]
          }
          else ();
          loop (d + 1);
        };
      flush stdout;
    };
    loop (n + 1);
  }
;

main ();

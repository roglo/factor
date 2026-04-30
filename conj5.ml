(* $Id: conj5.ml,v 1.5 2017/12/28 10:59:45 deraugla Exp $ *)

open Printf;

value is_utf8 =
  let lang =
    try Sys.getenv "LC_ALL" with
    [ Not_found ->
        try Sys.getenv "LC_MESSAGES" with
        [ Not_found ->
            try Sys.getenv "LANG" with
            [ Not_found -> "en" ] ] ]
  in
  match try Some (String.index lang '.') with [ Not_found -> None ] with
  [ Some i ->
      let s = String.sub lang (i + 1) (String.length lang - i - 1) in
      String.lowercase_ascii s = "utf-8"
  | None -> False ]
;

value e2 = if is_utf8 then "Â²" else "²";

value is_prime n =
  if n < 2 then False
  else
    loop 2 where rec loop d =
      let q = n / d in
      if q < d then True
      else if n mod d = 0 then False
      else loop (d + 1)
;

value main () =
  loop 2 where rec loop p = do {
    if is_prime p && p mod 4 = 1 then do {
      let i =
        loop 1 2 2 where rec loop r i i_pow_r =
          let _ = assert (i_pow_r > 0) in
          if r > 4 then loop 1 (i + 1) (i + 1)
          else if i_pow_r = 1 then
            if r = 4 then i
            else loop 1 (i + 1) (i + 1)
          else
            let i_pow_r = i_pow_r * i in
            loop (r + 1) i (i_pow_r mod p)
      in
      let (a, b) =
        loop 1 where rec loop a =
          let b = i * a mod p in
          let _ = assert (b > 0) in
          if a * a + b * b = p then (a, b)
          else if a * a + (p - b) * (p - b) = p then (a, p - b)
          else loop (a + 1)
      in
      assert ((i * i + 1) mod p = 0);
      let k = (i * i + 1) / p in
      printf "%d %d=%d%s+%d%s\ti=%d\ti%s=%sp-1\n" p p a e2 b e2 i e2
        (if k = 1 then "" else sprintf "%d" k);
      flush stdout;
    }
    else ();
    loop (p + 1)
  }
;

main ();

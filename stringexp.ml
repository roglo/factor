(* $Id: stringexp.ml,v 1.4 2017/12/28 10:59:45 deraugla Exp $ *)

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

value utf8_e =
  [| "\226\129\176"; "\194\185"; "\194\178"; "\194\179"; "\226\129\180";
     "\226\129\181"; "\226\129\182"; "\226\129\183"; "\226\129\184";
     "\226\129\185" |]
;

value f i =
  if not is_utf8 then "^" ^ string_of_int i
  else
    loop [] i where rec loop sl i =
      if i = 0 then String.concat "" sl
      else loop [utf8_e.(i mod 10) :: sl] (i / 10)
;

value e2 = if not is_utf8 then "\178" else f 2;
value e3 = if not is_utf8 then "\179" else f 3;

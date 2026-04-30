type num =
  [ Int of int
  | Big_int of Big_int.big_int ]
;

value num_of_string _ = failwith "not impl : Num.num_of_string";
value string_of_num _ = failwith "not impl : Num.string_of_num";
value big_int_of_num _ = failwith "not impl : Num.big_int_of_num";
value num_of_big_int _ = failwith "not impl : Num.num_of_big_int";

value pred_num _ = failwith "not impl : Num.pred_num";
value succ_num _ = failwith "not impl : Num.succ_num";
value add_num _ _ = failwith "not impl : Num.add_num";
value sub_num _ _ = failwith "not impl : Num.sub_num";
value mult_num _ _ = failwith "not impl : Num.mult_num";
value quo_num _ _ = failwith "not impl : Num.quo_num";
value mod_num _ _ = failwith "not impl : Num.mod_num";
value square_num _ _ = failwith "not impl : Num.square_num";

value eq_num (_ : num) (_ : num) = failwith "not impl : Num.eq_num";
value le_num _ _ = failwith "not impl : Num.le_num";
value ge_num _ _ = failwith "not impl : Num.ge_num";
value power_num _ _ = failwith "not impl : Num.power_num";


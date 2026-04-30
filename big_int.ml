type big_int = { sign : int; abs_value : nat };

value zero_big_int = { sign = 0; abs_value = make_nat 1 };
value unit_big_int = failwith "not impl: unit_big_int";

value big_int_of_int _ = failwith "not impl: big_int_of_int";
value big_int_of_nat _ = failwith "not impl: big_int_of_nat";
value big_int_of_string _ = failwith "not impl: big_int_of_string";
value int_of_big_int _ = failwith "not impl: int_of_big_int";
value nat_of_big_int _ = failwith "not impl: nat_of_big_int";
value string_of_big_int _ = failwith "not impl: string_of_big_int";

value compare_big_int _ _ = failwith "not impl: compare_big_int";

value eq_big_int _ _ = failwith "not impl: eq_big_int";
value lt_big_int _ _ = failwith "not impl: lt_big_int";
value le_big_int _ _ = failwith "not impl: le_big_int";
value gt_big_int _ _ = failwith "not impl: gt_big_int";
value ge_big_int _ _ = failwith "not impl: ge_big_int";

value abs_big_int _ = failwith "not impl: abs_big_int";
value min_big_int _ _ = failwith "not impl: min_big_int";
value minus_big_int _ = failwith "not impl: minus_big_int";

value add_big_int _ _ = failwith "not impl: add_big_int";
value sub_big_int _ _ = failwith "not impl: sub_big_int";
value mult_big_int _ _ = failwith "not impl: mult_big_int";

value quomod_big_int _ _ = failwith "not impl: quomod_big_int";
value div_big_int _ _ = failwith "not impl: div_big_int";
value mod_big_int _ _ = failwith "not impl: mod_big_int";
value gcd_big_int _ _ = failwith "not impl: gcd_big_int";

value add_int_big_int _ _ = failwith "not impl: add_int_big_int";
value mult_int_big_int _ _ = failwith "not impl: mult_int_big_int";

value power_big_int_positive_big_int _ _ =
  failwith "not impl: power_big_int_positive_big_int"
;
value power_big_int_positive_int _ _ =
  failwith "not impl: power_big_int_positive_int"
;
value power_int_positive_int _ _ =
  failwith "not impl: power_int_positive_int"
;

value square_big_int _ = failwith "not impl: square_big_int";
value sqrt_big_int _ = failwith "not impl: sqrt_big_int";

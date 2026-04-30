value create_nat _ = failwith "not impl: Nat.create_nat";
value length_nat _ = failwith "not impl: Nat.length_nat";
value blit_nat _ _ _ _ _ : unit = failwith "not impl: Nat.blit_nat";
value land_digit_nat _ _ _ _ = failwith "not impl: Nat.land_digit_nat";
value lor_digit_nat _ _ _ _ = failwith "not impl: Nat.lor_digit_nat";
value lxor_digit_nat _ _ _ _ = failwith "not impl: Nat.lxor_digit_nat";
value length_of_digit = Sys.word_size;
value num_leading_zero_bits_in_digit _ _ =
  failwith "not impl: Nat.num_leading_zero_bits_in_digit"
;

module Sbox = struct
  open Z

  let primitive = of_int 0x11b

  let gf_mul x y =
    let rec loop x y acc =
      if y = zero then acc
      else
        let acc = if (logand y one) <> zero then logxor acc x else acc in
        let y = shift_right y 1 in
        let x = shift_left x 1 in
        if (logand x (of_int 0x100)) <> zero then
          loop (logxor x primitive) y acc
        else
          loop x y acc
    in loop x y zero

  let gf_inv x =
    let rec egcd a b =
      if b = zero then (a, one, zero)
      else
        let q, r = div_rem a b in
        let g, x, y = egcd b r in
        (g, y, logxor x (gf_mul q y))
    in
    let g, x, _ = egcd x primitive in
    if g = one then x else zero

  let rotate_left x n =
    let n_int = Z.to_int n in
    let shift_amount = Z.to_int (Z.sub (Z.of_int 8) n) in
    Z.logor (Z.shift_left x n_int) (Z.shift_right x shift_amount)

  let affine_transformation x =
    let operations = [0; 1; 2; 3; 4; 5; 6; 7] in
    let b = of_int 0x63 in
    List.fold_left (fun acc i ->
      let bit = logand (shift_right x i) one in
      logxor acc (shift_left bit i)
    ) b operations

  let create_sbox_table () =
    let range = List.init 256 (fun i -> i) in
    List.map (fun i ->
      let inv = if i = 0 then zero else gf_inv (of_int i) in
      affine_transformation inv
    ) range
end

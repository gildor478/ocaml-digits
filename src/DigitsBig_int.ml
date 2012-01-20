
open Big_int

let of_big_int ?(base=10) n =
  let base_bi = big_int_of_int base in

  let rec of_big_int' acc n =
    let sign = sign_big_int n in
    if sign = 0 then
      acc
    else if sign > 0 then
      let q, r = 
        quomod_big_int n base_bi 
      in
        of_big_int' (int_of_big_int r :: acc) q
    else
      of_big_int' acc (minus_big_int n) 
  in
    of_big_int' [] n

let to_big_int ?(base=10) lst =
  List.fold_left
    (fun acc n ->
       add_big_int
         (mult_int_big_int base acc)
         (big_int_of_int n))
    zero_big_int
    lst



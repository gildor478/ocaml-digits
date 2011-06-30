
let of_int ?(base=10) n = 
  let rec of_int' acc n =
    if n = 0 then
      acc
    else if n > 0 then
      of_int' (n mod base :: acc) (n / base)
    else
      of_int' acc (-n) 
  in
    of_int' [] n

let to_int ?(base=10) lst =
  List.fold_left
    (fun acc n ->
       acc * base + n)
    0
    lst

let of_int32 ?(base=10) n =
  let base = Int32.of_int base in
  let rec of_int' acc n =
    if n = 0l then
      acc
    else if n > 0l then
      of_int' (Int32.to_int (Int32.rem n base) :: acc) (Int32.div n base)
    else
      of_int' acc (Int32.neg n) 
  in
    of_int' [] n

let of_int64 ?(base=10) n =
  let base = Int64.of_int base in
  let rec of_int' acc n =
    if n = 0L then
      acc
    else if n > 0L then
      of_int' (Int64.to_int (Int64.rem n base) :: acc) (Int64.div n base)
    else
      of_int' acc (Int64.neg n) 
  in
    of_int' [] n

let to_string lst =
  let buff = Buffer.create (List.length lst) in
    List.iter
      (fun d ->
         let code = 
           if d < 0 then
             invalid_arg "Digits.to_string"
           else if d < 10 then
             d + Char.code '0'
           else
             (d - 10) + Char.code 'a'
         in
           Buffer.add_char buff (Char.chr code))
      lst;
    Buffer.contents buff

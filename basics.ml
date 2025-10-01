let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  let (x, y) = tup in
  (y, x)
let rev_triple (tup : 'a * 'b * 'c) = 
  let (x, y, z) = tup in
  (z, y, x)

let is_odd x = 
  x mod 2 <> 0

let is_older (date1: int * int * int) (date2: int * int * int) = 
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (date1: int * int * int) = 
  let (y, m, d) = date1 in
  (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n = 
  if n = 0 then 1
  else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  match lst with
  | [] -> failwith "out of bounds"
  | h :: t -> if idx = 0 then h else get_nth (idx - 1, t)

let larger lst1 lst2 = 
  let rec len lst = 
    match lst with
    | [] -> 0
    | _ :: t -> 1 + len t
  in
  let l1 = len lst1 in
  let l2 = len lst2 in
  if l1 > l2 then lst1
  else if l2 > l1 then lst2
  else []

let sum lst1 lst2 = 
  let rec add_all lst = 
    match lst with
    | [] -> 0
    | h :: t -> h + add_all t
  in
  add_all lst1 + add_all lst2

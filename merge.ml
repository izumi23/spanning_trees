let rec divise l1 l2 = function
  | [] -> l1, l2
  | [a] -> a :: l1, l2
  | a :: b :: l -> let m1, m2 = divise l1 l2 l in a :: m1, b :: m2

  
let rec fusion f l1 l2 = match l1, l2 with
  | _, [] -> l1
  | [], _ -> l2
  | a :: m1, b :: m2 when f a b -> a :: fusion f m1 l2
  | _, b :: m2 -> b :: fusion f l1 m2


let rec sort f l = match l with
  | [] | [_] -> l
  | _ -> let m1, m2 = divise [] [] l in fusion f (sort f m1) (sort f m2)

let sort_int l =
  sort (fun a b -> a <= b) l
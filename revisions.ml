let rec divise l1 l2 = function
  | [] -> l1, l2
  | [a] -> a :: l1, l2
  | a :: b :: l -> let m1, m2 = divise l1 l2 l in a :: m1, b :: m2

  
let rec fusion l1 l2 = match l1, l2 with
  | _, [] -> l1
  | [], _ -> l2
  | a :: m1, b :: m2 when a <= b -> a :: fusion m1 l2
  | _, b :: m2 -> b :: fusion l1 m2


let rec tri_fusion l = match l with
  | [] | [_] -> l
  | _ -> let m1, m2 = divise [] [] l in fusion (tri_fusion m1) (tri_fusion m2)


let combinaison n k =
  let a = Array.make (n+1) 0 in
  a.(0) <- 1 ;
  for i = 1 to n do
    for j = i downto 1 do a.(j) <- a.(j-1) + a.(j) done
  done ;
  a.(k)

  
let sac_dos n c w wmax =
  let f = Array.make_matrix (n+1) (wmax+1) 0 in
  for i = 1 to n do
    for j = 0 to wmax do
      f.(i).(j) <- if w.(i) <= j then max (c.(n) + f.(i-1).(j-w.(i))) f.(i-1).(j) else f.(i-1).(j)
    done
  done ;
  f.(n).(wmax)


let rgb_tiles_or n =
  let nb_ways k =
    let a = Array.make (n+1) 0 in
    a.(0) <- 1 ;
    for i = 1 to n do
      a.(i) <- a.(i-1) ;
      if i >= k then a.(i) <- a.(i) + a.(i-k)
    done ;
    a.(n) - 1
  in
  nb_ways 2 + nb_ways 3 + nb_ways 4 


let carre_max a n =
  let m = ref 0 in
  let b = Array.make_matrix n n 0 in
  for i = 0 to n-1 do b.(i).(0) <- a.(i).(0) ; b.(0).(i) <- a.(0).(i) done ;
  for i = 1 to n-1 do
    for j = 1 to n-1 do
      b.(i).(j) <- a.(i).(j) * (1 + min a.(i-1).(j-1) (min a.(i-1).(j) a.(j-1).(i))) ;
      m := max !m b.(i).(j)
    done
  done ;
  !m


let parcours_largeur g =
  let dejavu = Array.make (Array.length g) false in
  let atraiter = Queue.create () in
  let explore_voisin y =
    if not dejavu.(y) then (Queue.add y atraiter ; dejavu.(y) <- true)
  in
  explore_voisin 0 ;
  while not (Queue.is_empty atraiter) do
    let x = Queue.take atraiter in
    print_int x ;
    List.iter explore_voisin g.(x)
  done

let graphe_exemple = [| [1; 3; 5];[3; 4];[3];[4];[];[1] |]


let parcours_profondeur g =
  let dejavu = Array.make (Array.length g) false in
  let rec aux = function
    | [] -> ()
    | x :: l when not dejavu.(x) ->
        dejavu.(x) <- true ; print_int x ; aux g.(x) ; aux l
    | x :: l -> aux l
  in print_int 0 ; aux g.(0) 



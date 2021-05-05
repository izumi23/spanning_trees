let boruvka g =

  let n = Array.length g in
  let c = Unionfind.create n in
  let e = Array.make n (0, 0, max_float) in

  let consider_edge x y p =
    let i = Unionfind.component c x and j = Unionfind.component c y in
    let a, b, w = e.(i) in
    if i != j && p < w then e.(i) <- (x, y, p)
  in

  let rec add_edges m s l i =
    if i = n || m = n-1 then m, s, l
    else
      let x, y, p = e.(i) in
      if p = max_float then add_edges m s l (i+1)
      else (
        e.(i) <- (0, 0, max_float) ;
        let i1 = Unionfind.component c x and j = Unionfind.component c y in
        if i1 = j then add_edges m s l (i+1)
        else (
          Unionfind.merge c i1 j ;
          add_edges (m+1) (s +. p) ((x, y) :: l) (i+1)
        )
      )
  in

  let rec aux m s l x = function
    | _ when m >= n-1 -> s, List.rev l
    | [] when x = n-1 ->
        let m1, s1, l1 = add_edges m s l 0 in aux m1 s1 l1 0 g.(0)
    | [] -> aux m s l (x+1) g.(x+1)
    | (y, p) :: q -> consider_edge x y p ; aux m s l x q
  in  
  aux 0 0. [] 0 g.(0)

;;

(* let s, t = boruvka Graph.g1 in
Printf.printf "%f, " s ; Print.print_list_pairs t *)
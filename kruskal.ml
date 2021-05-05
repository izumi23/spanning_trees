let kruskal g =
  let n = Array.length g in
  let l = Merge.sort (fun (_,_,p) (_,_,q) -> p <= q) (Graph.array_to_list g) in
  print_string "sorted\n" ; flush stdout ;
  let c = Unionfind.create n in
  let rec aux w t = function
    | [] -> w, List.rev t
    | (x, y, p) :: l1 ->
        let i = Unionfind.component c x and j = Unionfind.component c y in
        if i != j then (Unionfind.merge c i j ; aux (w +. p) ((x, y) :: t) l1)
        else aux w t l1
  in
  aux 0. [] l

;;  


(* let s, t = kruskal Graph.g1 in
Printf.printf "%f, " s ; Print.print_list_pairs t *)



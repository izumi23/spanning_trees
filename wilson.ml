let wilson_complete n =

  let visite = Array.make n false in
  let parent = Array.make n (-1) in
  visite.(0) <- true ;
  
  let rec find_way l x =
    if visite.(x) then List.rev (x :: l)
    else find_way (x :: l) (Random.int n)
  in

  let add_edges l =
    Print.print_list l ;
    let a = Array.of_list l in
    let k = Array.length a in
    let h = Hashtbl.create k in
    for i = k-1 downto 0 do
      if not (Hashtbl.mem h a.(i)) then Hashtbl.add h a.(i) i
    done ;
    let rec retrace prev i =
      if i < k then (
        let j = Hashtbl.find h a.(i) in
        if prev >= 0 then (visite.(prev) <- true ; parent.(prev) <- a.(i)) ;
        retrace a.(i) (j+1)
      )
    in
    retrace (-1) 0 ;
  in

  for i = 1 to n-1 do
    if not visite.(i) then add_edges (find_way [] i)
  done ;
  parent

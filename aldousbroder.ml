let aldous_broder g =

  let n = Array.length g in
  let visite = Array.make n false in
  let parent = Array.make n (-1) in
  visite.(0) <- true ;
  
  let rec aux m l x =
    if m = n-1 then List.rev l, parent
    else
      let d = List.length g.(x) in
      let r = Random.int d in
      let (y, _) = List.nth g.(x) r in
      if visite.(y) then aux m l y
      else (
        visite.(y) <- true ; parent.(y) <- x ;
        aux (m+1) ((x, y) :: l) y
      )
    
  in
  aux 0 [] 0


let aldous_broder_complete n =

  let visite = Array.make n false in
  let parent = Array.make n (-1) in
  visite.(0) <- true ;
  
  let rec aux m l x =
    if m = n-1 then List.rev l, parent
    else
      let y = Random.int n in
      if visite.(y) then aux m l y
      else (
        visite.(y) <- true ; parent.(y) <- x ;
        aux (m+1) ((x, y) :: l) y
      )
    
  in
  aux 0 [] 0

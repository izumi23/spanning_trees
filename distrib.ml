let simul_poisson lambda =
  let rec aux i x =
    if x < exp (-. lambda) then i-1
    else aux (i+1) (x *. Random.float 1.)
  in
  aux 0 1.
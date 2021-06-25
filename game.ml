open Gamegraphics


type game_state = {
  mutable last_known : int
}


let new_game_env m n p delay = {
  height = m ;
  width = n ;
  square_size = min 72 (720/m) ;
  path = p ;
  orientation = 0 ;
  moves = [] ;
  wait = fun () -> Unix.sleepf delay
}


let new_game_state () = {
  last_known = 1
}


let rec wait_for_input () =
  while Graphics.key_pressed () do ignore (Graphics.read_key ()) done ;
  let k = Graphics.read_key () in
  match k with 
    | '0' | 't' -> Toggle
    | 's' -> Backbite 0
    | 'z' -> Backbite 3
    | 'd' -> Backbite 2
    | 'q' -> Backbite 1
    | 'n' | ' ' -> Strat 0
    | 'a' -> Strat 4
    | 'f' -> Strat 3
    | 'r' -> Strat 4
    | 'g' -> Strat 5
    | _ -> wait_for_input ()


let execute env move = 
  let rec aux = function
    | Toggle -> env.orientation <- 1 - env.orientation
    | Backbite dir ->
        Markovpath.transition env.path env.orientation dir
    | _ -> ()
  in
  aux move ; draw_path env


let execute_all env l =
  let execute_with_pause env move =
    execute env move ; env.wait ()
  in
  List.iter (execute_with_pause env) l


let goal_path_next x n =
  let y = Markovpath.default_path_next x n in
  if y mod n = 0 then y + n + 1 else y


let check_last_known env state =
  let p = env.path in
  let m = p.height and n = p.width in
  let rec aux x =
    let y = goal_path_next x n in
    if y < m*n && abs (p.index.(x) - p.index.(y)) = 1 then (
      state.last_known <- y ;
      aux y
    )
  in
  aux state.last_known


let step_along_cycle env =
  match Markovpath.direction_other_end env.path env.orientation with
    | None -> ()
    | Some dir -> execute_all env [Backbite dir; Toggle]


let walk_along_cycle env state =
  let m = env.height and n = env.width in
  check_last_known env state ;
  let goal = goal_path_next state.last_known n in
  let rec aux () =
    if env.path.node.((m*n-1) * env.orientation) != goal then
      match Markovpath.direction_other_end env.path env.orientation with
      | None -> ()
      | Some dir -> execute_all env [Backbite dir; Toggle] ; aux ()
  in
  aux () ;
  let goal_up = Markovpath.displacement 3 goal m n in
  let other_end = (m*n-1) * (1 - env.orientation) in
  if env.path.node.(other_end) = goal_up then
    execute_all env [Toggle; Backbite 0]

    
let pull_path env way =
  let seq = 
    if way = 0 then [Backbite 0; Toggle; Backbite 1; Backbite 2]
    else [Backbite 0; Toggle; Backbite 2; Backbite 1]
  in
  execute_all env seq


let advance env state =
  let m = env.height and n = env.width in
  let p = env.path in
  check_last_known env state ;
  let x = state.last_known in
  let dir = if (x / n) mod 2 = 0 then 2 else 1 in
  let y = Markovpath.displacement dir x m n in
  let z = Markovpath.displacement 3 x m n in
  let cond o =
    p.index.(y) = o*(m*n-1) && (2*o-1)*(p.index.(x) - p.index.(z)) < 0
  in
  if cond env.orientation then
    execute env (Backbite (3-dir))
  else if cond (1 - env.orientation) then
    execute_all env [Toggle; Backbite (3-dir)]


let rec automatic_strat env state =
  let rec aux () =
    check_last_known env state ;
    let lk = state.last_known in
    if lk / env.width < env.height - 1 then (
      advance env state ;
      if state.last_known != lk then aux ()
      else (
        walk_along_cycle env state ;
        advance env state ;
        if state.last_known = lk then game_loop env state ;
        aux ()
      )
    )
  in
  aux ()


and strat env state = function
  | 0 -> step_along_cycle env
  | 1 -> pull_path env 0
  | 2 -> pull_path env 1
  | 3 -> advance env state
  | 4 -> walk_along_cycle env state
  | _ -> automatic_strat env state


and game_loop env state =
  let input_move = wait_for_input () in
  match input_move with
  | Toggle | Backbite _ -> execute env input_move
  | Strat k -> strat env state k

  
let play m n p delay =
  let env = new_game_env m n p delay in
  let state = new_game_state () in
  init env ;
  draw_path env ;
  while true do game_loop env state done

(* 
  let x = state.last_known in let y = disp 3 x in
  let z = disp (3-env.orientation) x in let t = disp 3 z in
  let i = env.path.index in
  if (i.(x)-i.(y)) * (i.(z)-i.(t)) *)
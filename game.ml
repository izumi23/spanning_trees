open Gamegraphics


let new_game_env m n p delay = {
  height = m ;
  width = n ;
  square_size = 72 ;
  path = p ;
  orientation = 0 ;
  moves = [] ;
  wait = fun () -> Unix.sleepf delay
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
    | 'e' -> Strat 1
    | 'a' -> Strat 2
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


let walk_along_cycle env =
  match Markovpath.direction_other_end env.path env.orientation with
  | None -> ()
  | Some dir -> execute_all env [Backbite dir; Toggle]

    
let pull_path env way =
  let seq = 
    if way = 0 then [Backbite 0; Toggle; Backbite 1; Backbite 2]
    else [Backbite 0; Toggle; Backbite 2; Backbite 1]
  in
  execute_all env seq


let strat env = function
  | 0 -> walk_along_cycle env
  | 1 -> pull_path env 0
  | _ -> pull_path env 1


let game_loop env =
  let input_move = wait_for_input () in
  match input_move with
  | Toggle | Backbite _ -> execute env input_move
  | Strat k -> strat env k

  
let play m n p delay =
  let env = new_game_env m n p delay in
  init env ;
  draw_path env ;
  while true do game_loop env done
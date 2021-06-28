type move =
  | Toggle
  | Strat of int
  | Backbite of int


type game_environment = {
  height : int ;
  width : int ;
  square_size : int ;
  path : Markovpath.rooted_path ;
  mutable orientation : int ;
  mutable moves : move list ;
  wait : unit -> unit 
}


let repaint env =
  let xs = env.square_size * (env.width + 2) in
  let ys = env.square_size * (env.height + 2) in
  Graphics.set_color Graphics.black ;
  Graphics.fill_rect 0 0 xs ys


let init env =
  let xs = env.square_size * (env.width + 1) in
  let ys = env.square_size * (env.height + 1) in
  let x_offset = (1920 - xs)/2 and y_offset = (1080 - ys)/2 in
  let dimensions = Printf.sprintf " %dx%d+%d+%d" xs ys x_offset y_offset in
  Graphics.open_graph dimensions ;
  Graphics.set_window_title "Hamiltonian path game" ;
  Graphics.auto_synchronize false ;
  repaint env ;
  Graphics.set_line_width (min 10 (100/env.path.height)) 


let point square_size x y =
  square_size*(x+1), square_size*(y+1)


let draw_line sq n i j color =
  let x0, y0 = point sq (i mod n) (i/n) in
  let x1, y1 = point sq (j mod n) (j/n) in
  Graphics.set_color color ;
  Graphics.moveto x0 y0 ;
  Graphics.lineto x1 y1


let draw_endpoint sq n i orientation =
  let x, y = point sq (i mod n) (i/n) in
  Graphics.set_color Graphics.(if orientation = 0 then green else white) ;
  Graphics.fill_circle x y (min 20 (400/n))


let draw_path env lk =
  repaint env ;
  let p = env.path and sq = env.square_size in
  let m = env.height and n = env.width in
  let e = p.index.(1) - p.index.(0) in
  for i = 0 to m*n - 2 do
    let i1 = i + (1-e)/2 in
    let color = Graphics.(if e*(i1 - p.index.(lk)) < 0 then
      if e*(i1 - p.index.(1)) < 0 then white else cyan
      else magenta
    )
    in 
    draw_line sq n p.node.(i) p.node.(i+1) color
  done ;
  draw_endpoint sq n p.node.(0) env.orientation ;
  draw_endpoint sq n p.node.(m*n-1) (1 - env.orientation) ;
  Graphics.synchronize ()

  
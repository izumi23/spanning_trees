let square_size = 72

let repaint side =
  Graphics.set_color Graphics.black ;
  Graphics.fill_rect 0 0 side side ;
  Graphics.set_color Graphics.white

let init s =
  let side = square_size*(s+1) in
  let x_offset = (1920 - side)/2 and y_offset = (1080 - side)/2 in
  let dimensions = Printf.sprintf " %dx%d+%d+%d" side side x_offset y_offset in
  Graphics.open_graph dimensions ;
  Graphics.set_window_title "Hamiltonian path game" ;
  Graphics.auto_synchronize false ;
  repaint side ;
  Graphics.set_line_width 10

let point x y =
  square_size*(x+1), square_size*(y+1)

let draw_line s i j =
  let x0, y0 = point (i mod s) (i/s) in
  let x1, y1 = point (j mod s) (j/s) in
  Graphics.moveto x0 y0 ;
  Graphics.lineto x1 y1

let draw_endpoint s i orientation =
  let x, y = point (i mod s) (i/s) in
  if orientation = 0 then Graphics.set_color Graphics.green ;
  Graphics.fill_circle x y 20 ;
  if orientation = 0 then Graphics.set_color Graphics.white

let draw_path parent root leaf orientation =
  let n = Array.length parent in
  let s = int_of_float (sqrt (float_of_int n)) in
  repaint (square_size*(s+1)) ;
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j >= 0 then draw_line s i j
  done ;
  draw_endpoint s root orientation ;
  draw_endpoint s leaf (1 - orientation) ;
  Graphics.synchronize ()

let wait_for_input () =
  let k = Graphics.read_key () in
  match k with 
    | 't' -> true, 0
    (* | '\001' -> false, 0
    | '\002' -> false, 3
    | '\003' -> false, 2
    | '\004' -> false, 1
    | _ -> false, -1 *)
    | 's' -> false, 0
    | 'z' -> false, 3
    | 'd' -> false, 2
    | 'q' -> false, 1
    | _ -> false, -1
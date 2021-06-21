let write_tree parent file =
  let out = open_out file in
  let p = Printf.fprintf in
  let n = Array.length parent in
  let s = int_of_float (sqrt (float_of_int n)) in
  for i = 0 to n-1 do
    let j = parent.(i) in
    if j >= 0 && (abs (i-j) = 1 || abs (i-j) = s) then
      p out "%d %d\n%d %d\n\n" (i mod s) (i/s) (j mod s) (j/s)
  done ;
  close_out_noerr out


let plot_tree g filename =
  Sys.chdir "plots" ;
  write_tree g (filename ^ ".dat") ;
  Printf.printf "Wrote plots/%s.dat\n" filename ; flush stdout ;
  let s = int_of_float (sqrt (float_of_int (Array.length g))) in
  let comd = Printf.sprintf
    "gnuplot -p -e \"filename='%s.dat'; fileout='%s.png'; sze=%d\" config.gnu"
    filename filename (3*s-2)
  in
  Printf.printf "%s\n" comd ; flush stdout ;
  ignore (Sys.command comd) ;
  Printf.printf "Wrote plots/%s.png\n\n" filename ;
  Sys.chdir ".."
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 1 pointtype 7 pointsize 0
set terminal png enhanced background rgb 'black' size sze,sze
unset tics
unset border
set key noautotitle
set size square
set tmargin 0
set bmargin 0
set lmargin 0
set rmargin 0
set output fileout

plot filename with linespoints linestyle 1

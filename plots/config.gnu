set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 1 pointtype 7 pointsize 0
set terminal png enhanced background rgb 'black' 
unset tics
unset border
set key noautotitle
set size square
set output fileout

plot filename with linespoints linestyle 1
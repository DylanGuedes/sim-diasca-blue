set autoscale
unset log
set grid
set style data linespoints
set style fill empty
set key box bmargin center horizontal
set pointsize 1
set xtic auto
set ytic auto
# No xrange set.
# No yrange set.
set title "Monitoring the soda consumption"
set xlabel "Simulation tick"
set ylabel "Number of cans still available in the machine"
set datafile missing 'undefined'
set terminal png size 1600, 800

set output "First_soda_machine_Soda_Stock_Probe.png"
plot  "First_soda_machine_Soda_Stock_Probe.dat" using 1:2 title "First soda machine can stock" noenhanced

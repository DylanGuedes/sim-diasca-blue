set autoscale
unset log
set grid
set style data linespoints
set style fill empty
set key box bmargin center horizontal
set pointsize 1
set xtic rotate by - 45
set ytic auto
# No xrange set.
# No yrange set.
set title "Monitoring the Overall and Per-Class Instance Count Over Simulation Ticks"
set xlabel "Simulation time, in ticks"
set ylabel "Instance Count"
set datafile missing 'undefined'
set terminal png size 1600, 800

set output "Per_Class_Instance_Count_Over_Tick_Probe.png"
plot  "Per_Class_Instance_Count_Over_Tick_Probe.dat" using 1:2 title "Overall Instance Count" noenhanced

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
set title "Monitoring the Overall and Per-Node Instance Count Over Wall-clock Time"
set xlabel "Wall-clock duration, in milliseconds"
set ylabel "Instance Count"
set datafile missing 'undefined'
set terminal png size 1600, 800

set output "Per_Node_Instance_Count_Over_Time_Probe.png"
plot  "Per_Node_Instance_Count_Over_Time_Probe.dat" using 1:2 title "Overall Instance Count" noenhanced

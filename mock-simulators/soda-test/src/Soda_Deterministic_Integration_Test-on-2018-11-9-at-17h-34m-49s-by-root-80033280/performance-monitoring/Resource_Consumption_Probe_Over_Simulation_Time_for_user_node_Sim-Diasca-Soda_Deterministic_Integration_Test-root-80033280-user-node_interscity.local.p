set autoscale
unset log
set grid
set style data linespoints
set style fill empty
set key box bmargin center
set pointsize 2
set xtic rotate by - 45
set ytic auto
# No xrange set.
set yrange [0:100]
set title "Monitoring memory and swap consumptions on user node Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node@interscity.local over wall-clock time"
set xlabel "Simulation duration, in simulation ticks"
set ylabel "Percentages of Memory Consumption, Swap and CPU Use and Process Count"
set datafile missing 'undefined'
set terminal png size 1600, 800

set output "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.png"
plot  "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:2 with filledcurves x2 title "Total available memory (free+buffers+cache)", "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:2:3 with filledcurves title "Memory used by all other programs", "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:3 with filledcurves x1 title "Memory used for the simulation (over a total of 15.6 GiB)", "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:4 title "Swap Used (in GiB, over a total of 9.0 GiB)" noenhanced, "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:5 title "Percentage of CPU Used (Non-idle)" noenhanced, "Resource_Consumption_Probe_Over_Simulation_Time_for_user_node_Sim-Diasca-Soda_Deterministic_Integration_Test-root-80033280-user-node_interscity.local.dat" using 1:6 title "Erlang Process Count (spread over 4 cores)" noenhanced

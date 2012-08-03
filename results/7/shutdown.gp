set terminal png 
#set ydata time
#set timefmt y "%Y%m%dT%H:%M:%SZ"
set title "Time to hard shutdown lots of miniOS VMs with NFS driver domain"
set ylabel "Wall clock time (seconds)"
set xlabel "VM index"

plot "4-worker-shutdown-before-speedups-normalised.dat" using 1:2:2:3 with yerrorbars title "trunk", \
     "4-worker-shutdown-normalised.dat" using 1:2:2:3 with yerrorbars title "with fixes"

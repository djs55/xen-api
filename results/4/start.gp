set terminal png 
#set ydata time
#set timefmt y "%Y%m%dT%H:%M:%SZ"
set title "Time to boot lots of miniOS VMs with NFS driver domain"
set ylabel "Wall clock time (seconds)"
set xlabel "VM index"

plot "1-worker-start-normalised.dat" using 1:2:2:3 with yerrorbars title "1 thread + patches", \
     "1-worker-start-before-speedups-normalised.dat" using 1:2:2:3 with yerrorbars title "1 thread" , \
     "2-worker-start-normalised.dat" using 1:2:2:3 with yerrorbars title "2 threads + patches", \
     "4-worker-start-normalised.dat" using 1:2:2:3 with yerrorbars title "4 threads + patches", \
     "4-worker-start-before-speedups-normalised.dat" using 1:2:2:3 with yerrorbars title "4 threads"

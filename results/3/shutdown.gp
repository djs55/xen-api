set terminal png 
set ydata time
set timefmt y "%Y%m%dT%H:%M:%SZ"
set title "Time to force-shutdown lots of miniOS VMs with NFS driver domain"
set ylabel "Wall clock time (MM:SS)"
set xlabel "VM index"

plot "shutdown.dat" using 0:3:3:4 with yerrorbars title "VM shutdown time"

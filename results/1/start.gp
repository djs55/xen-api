set terminal postscript
set ydata time
set timefmt y "%Y%m%dT%H:%M:%SZ"
set title "Time to boot lots of miniOS VMs with NFS driver domain"
set ylabel "Wall clock time"
set xlabel "VM index"

plot "start.dat" using 0:3:3:4 with yerrorbars title "VM start time"

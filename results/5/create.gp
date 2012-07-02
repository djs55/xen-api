set terminal png

set title "Time to create VMs with NFS driver domain"
set ylabel "Time/seconds"
set xlabel "VM index"
set timefmt "%s"
set ydata time

plot "600-create.dat" using 2:1 title "Time to create"

#!/bin/sh

# Usage:
# apiperf.sh <hostname>

hostname=$1
apiperf=/tmp/apiperf

echo "Sharing authorized_keys"
scp ~/.ssh/authorized_keys root@${hostname}:/root/.ssh/authorized_keys

echo "Running HTTP tests on $hostname"
ssh root@${hostname} "$apiperf -master -u root -pw xenroot -url http://${hostname} > ${hostname}.http.dat"
echo "Running HTTPS tests against $hostname"
ssh root@${hostname} "$apiperf -master -u root -pw xenroot -url https://${hostname} > ${hostname}.https.dat"
scp root@${hostname}:${hostname}.http.dat .
scp root@${hostname}:${hostname}.https.dat .
echo "Calculating PDFs"
cat ${hostname}.http.dat | weierstrass.pl 1% > ${hostname}.http.dat.out
cat ${hostname}.https.dat | weierstrass.pl 1% > ${hostname}.https.dat.out
cat > ${hostname}.gp <<EOF
set ylabel "Probability density"
set xlabel "Latency / milliseconds"
set title "XenAPI local call latency via HTTP and HTTPS on ${hostname}"
plot "${hostname}.http.dat.out" with lines title "HTTP", \
     "${hostname}.https.dat.out" with lines title "HTTPS"
EOF
echo "gnuplot -persist ${hostname}.gp"
echo "  will display the results"

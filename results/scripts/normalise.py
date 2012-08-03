#!/usr/bin/env python

# Read in 'start.dat' which contains lines like:
#   <vm name> <time command started> <time command returned>
# and 'ip.dat' which contains lines like:
#   <vm name> <time IP address was seen>

# produces a combined datafile which contains lines like:
#   <int index> <time command started> <time command returned> <time IP address was seen>
# ordered by <time command started>
# also produces a combined datafile which contains lines like:
#   <int index> <time guest booted>
# ordered by <time guest booted>

import sys, time, math

def read_whole_file(name):
    f = open(name, "r")
    try:
        return f.readlines()
    finally:
        f.close()

iso8601 = "%Y%m%dT%H:%M:%SZ"

hms = "%H:%M:%S"

def parse_time(x):
    global iso8601
    return time.mktime(time.strptime(x, iso8601))

def time_subtract(a, b):
    global hms
    aa = parse_time(a)
    bb = parse_time(b)
    s = aa - bb
    return s
    #return time.strftime(hms, time.gmtime(s))

records = {}

if __name__ == "__main__":
    if len(sys.argv) <> 2:
        print "Usage:"
        print sys.argv[0], " <start.dat>"
        sys.exit(1)
    start = sys.argv[1]
        
    start_data = read_whole_file(start)
    if start_data == None:
        raise "Failed to read file: %s" % start

    for line in start_data:
        line = line.strip()
        if line == "":
            break
        (result, name, vm_start_start, vm_start_end) = line.split()
        if result <> "SUCCESS":
            break
        records[name] = { 'vm_start_start': vm_start_start, 'vm_start_end': vm_start_end }
    # VM names are unimportant now

    # Find the earliest vm_start_start time so we can make all times relative

    times = records.values()

    earliest = times[0]['vm_start_start']
    vm_start_sigma_x = 0.0
    vm_start_sigma_xx = 0.0
    for rec in times:
        if rec['vm_start_start'] < earliest:
            earliest = rec['vm_start_start']

    times = sorted(times, lambda x,y:cmp(x['vm_start_start'], y['vm_start_start']))

    idx = 0
    for rec in times:
        print "%d %s %s" % (idx, time_subtract(rec['vm_start_start'], earliest), time_subtract(rec['vm_start_end'], earliest))
        idx = idx + 1    

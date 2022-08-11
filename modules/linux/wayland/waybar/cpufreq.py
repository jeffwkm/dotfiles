#!/usr/bin/env python3

# import sys, os
import subprocess
from subprocess import Popen, PIPE, run

# reads current MHz speed reported by kernel from cpupower utility
def read_cpufreq_mhz():
    p = Popen(["cpupower frequency-info -f | grep current | head -n1"],
              shell=True, encoding='utf8', stdout=PIPE)
    #p = Popen(["sudo cpupower frequency-info -f | grep current | head -n1"],
    #          shell=True, encoding='utf8', stdout=PIPE)
    result = p.communicate()[0].strip()
    value = result.split(': ')[1].split(' ')[0]
    return int( int(value) / 1000 )

# reads array of current MHz speeds for all cpu cores from /proc/cpuinfo
def read_cpuinfo_mhz():
    lines = [line.strip() for line in open('/proc/cpuinfo').readlines()
            if (line.find('MHz') >= 0)]
    tokens = [line.split(' ')[2] for line in lines]
    mhz = [int(float(s)) for s in tokens]
    return mhz

def process_cpuinfo():
    # get all mhz values
    all = read_cpuinfo_mhz()

    # sort by speed descending
    sorted = all
    sorted.sort()
    sorted.reverse()

    # take top half of values to exclude any inactive cores
    n = int( len(all)/2 )
    fast = sorted[0:n]
    avg = 0
    max = 0
    for x in fast:
        avg += x
        if x > max:
            max = x
    avg = int(avg / n)
    return {"max": max, "avg": avg, "cpu": sorted}

cpufreq = read_cpufreq_mhz()
print( "kernel\t%d" % cpufreq )
cpuinfo = process_cpuinfo()
print( "max\t%d" % cpuinfo["max"] )
print( "avg\t%d" % cpuinfo["avg"] )
for i in range(len(cpuinfo["cpu"])):
    print( "%d\t%d" % (i, cpuinfo["cpu"][i]) )

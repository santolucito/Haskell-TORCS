#!/usr/bin/gnuplot

set nokey
set t wxt persist
set datafile separator ","
plot 'test.csv' using 1:2:($0+1) pt 7 with linespoints,\
     'test.csv' every 2 using 1:2:($0+1) with labels offset 1

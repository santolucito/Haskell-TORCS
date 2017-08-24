#!/usr/bin/gnuplot

set t wxt persist
set datafile separator ","
plot 'test.csv' using 1:2:($0+1) with labels offset 1

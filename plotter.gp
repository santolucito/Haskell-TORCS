#!/usr/bin/gnuplot

set t wxt persist
set datafile separator ","
plot 'test.csv' using 1:2

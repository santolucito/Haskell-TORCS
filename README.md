#Haskell Binding to TORCS

requires 

- torcs 1.3.4 
- scr-server (https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/)

scr-server will only work with 1.3.4, until that is updated you must use exactly torcs 1.3.4

on Ubunutu 16.04, you may need to comment out line 70 in src/modules/simu/simuv2/simu.cpp 

TODO

- write controller for basic platoon
- build and ship to hackage

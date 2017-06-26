#Haskell Binding to TORCS

requires 

- torcs 1.3.4 (http://prdownloads.sourceforge.net/torcs/torcs-1.3.4.tar.bz2?download)
- scr-server 2.1 (https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/)

scr-server will only work with 1.3.4, until that is updated you must use exactly torcs 1.3.4

Complete install instructions here https://arxiv.org/abs/1304.1672

on Ubunutu 16.04, you may need to comment out line 70 in src/modules/simu/simuv2/simu.cpp 

TODO

- write controller for basic platoon
- build and ship to hackage

# Using machine learning with TORCS

Race Mode : Practice
Configure Race -> Last page of options, switch 'display' from 'normal' to 'results only'

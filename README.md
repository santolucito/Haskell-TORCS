#Haskell Binding to TORCS

requires 

- torcs 1.3.4 (http://prdownloads.sourceforge.net/torcs/torcs-1.3.4.tar.bz2?download)
- scr-server 2.1 (https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/)

scr-server will only work with 1.3.4, until that is updated you must use exactly torcs 1.3.4

Complete instructions here https://arxiv.org/abs/1304.1672

on Ubunutu 16.04, you may need to comment out line 70 in src/modules/simu/simuv2/simu.cpp 

## Usage

See the examples folder for how to program a controller.

You can load up the examples and run them with 'cabal repl' on the top directory. 
This will load TORCS.hs, which loads a few examples that can be connected to TORCS.

TODO

- write controller for basic platoon
- build and ship to hackage

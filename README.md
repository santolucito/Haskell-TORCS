# Haskell Binding to TORCS

[![Alt text](https://img.youtube.com/vi/xl16tnyAsoc/0.jpg)](https://www.youtube.com/watch?v=xl16tnyAsoc)


requires 

- torcs 1.3.4 (http://prdownloads.sourceforge.net/torcs/torcs-1.3.4.tar.bz2?download)
- scr-server 2.1 (https://sourceforge.net/projects/cig/files/SCR%20Championship/Server%20Linux/)

scr-server will only work with 1.3.4, until that is updated you must use exactly torcs 1.3.4

Complete TORCS install instructions here https://arxiv.org/abs/1304.1672

on Ubunutu 16.04, you may need to comment out line 70 in src/modules/simu/simuv2/simu.cpp 

## Usage

To install, go to the top level directory, and run 'cabal install'.
You only need to do 'cabal install' the first time, or whenever you change the cabal file.
Later on, just 'cabal repl' is fine.

See the examples folder for how to program a controller.

You can load up the examples and run them with 'cabal repl' on the top directory. 
This will load TORCS.hs, which loads a few examples that can be connected to TORCS.
As an example, first open TORCS to a quick race and set the player to src_server1. 
Then on the command line run:

    $> cabal repl
    ghci> :l Examples/Simple.hs
    ghci> main


### Using machine learning with TORCS

Race Mode : Practice
Configure Race -> Last page of options, switch 'display' from 'normal' to 'results only'

### TODO

X write controller for basic platoon
X build and ship to hackage

### Papers

To read more about Haskell-TORCS see - https://dl.acm.org/citation.cfm?doid=3055378.3055385

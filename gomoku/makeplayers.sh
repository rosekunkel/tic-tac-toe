#!/bin/bash

cd src
ghc --make MakePlayers.hs  > /dev/null 2>&1
./MakePlayers
rm MakePlayers
rm *.hi
rm *.o
cd ..
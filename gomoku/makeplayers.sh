#!/bin/bash

cd src
stack ghc -- --make MakePlayers.hs > /dev/null 2>&1
./MakePlayers
rm MakePlayers
rm *.hi
rm *.o
cd ..
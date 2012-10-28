#!/bin/sh

mkdir -p bin
cd src && ghc Main.hs -o ../bin/deturgenchry

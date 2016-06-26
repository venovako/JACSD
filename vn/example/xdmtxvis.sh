#!/bin/bash
cp -v ../cmap/plt8/jet8.txt dmtxvis.plt
ifort -mkl -standard-semantics -static-intel xdmtxvis.f90 -o xdmtxvis.exe -L../.. -lvn
./xdmtxvis.exe
cmp dmtxvis-0000000000.bmp dmtxvis.bmp

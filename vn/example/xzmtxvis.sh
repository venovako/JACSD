#!/bin/bash
cp -v ../cmap/plt8/jet8.txt zmtxvis.plt
if [ `uname` = "Darwin" ]
then
    gfortran -O3 -cpp -fdefault-integer-8 -fopenmp -march=native -Wa,-q xzmtxvis.f90 -o xzmtxvis.exe -L../.. -lvn -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -L${MKLROOT}/../compiler/lib -Wl,-rpath,${MKLROOT}/../compiler/lib -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -liomp5 -lpthread -lm -ldl
else
    gfortran -O3 -cpp -fdefault-integer-8 -fopenmp -march=native xzmtxvis.f90 -o xzmtxvis.exe -L../.. -lvn -L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -L${MKLROOT}/../compiler/lib/intel64 -Wl,-rpath=${MKLROOT}/../compiler/lib/intel64 -Wl,--no-as-needed -lmkl_gf_ilp64 -lmkl_intel_thread -lmkl_core -lpthread -lm -ldl
fi
./xzmtxvis.exe
cmp zmtxvisA0000000000.bmp zmtxvisA.bmp
cmp zmtxvisR0000000000.bmp zmtxvisR.bmp

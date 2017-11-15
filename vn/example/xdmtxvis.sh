#!/bin/bash
cp -v ../cmap/plt8/jet8.txt dmtxvis.plt
if [ `uname` = "Darwin" ]
then
    gfortran -O3 -cpp -march=native -fopenmp -Wa,-q xdmtxvis.f90 -o xdmtxvis.exe -L../.. -lvn -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -L${MKLROOT}/../compiler/lib -Wl,-rpath,${MKLROOT}/../compiler/lib -lmkl_intel_lp64 -lmkl_core -lmkl_intel_thread -liomp5 -lpthread -lm -ldl
else
    gfortran -O3 -cpp -march=native -fopenmp xdmtxvis.f90 -o xdmtxvis.exe -L../.. -lvn -L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -L${MKLROOT}/../compiler/lib/intel64 -Wl,-rpath=${MKLROOT}/../compiler/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_intel_thread -lmkl_core -lpthread -lm -ldl
fi
./xdmtxvis.exe
cmp dmtxvis-0000000000.bmp dmtxvis.bmp

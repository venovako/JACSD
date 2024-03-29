#!/bin/sh
# this is for macOS on Intel
icc -std=c18 -DNDEBUG -DUSE_MKL -DMKL_ILP64 -I.. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include gen_cbar.c -o gen_cbar.exe -L../.. -lvn -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -liomp5 -lpthread -lm -ldl
./gen_cbar.exe -6.60000000000000000E+01 5.84962500721172063E-01 32 520 1024 plt8/jet8.txt cbar_ex_32.bmp lg > cbar_ex_32.sh
chmod +x cbar_ex_32.sh
./cbar_ex_32.sh
rm -fv cbar_ex_32.sh

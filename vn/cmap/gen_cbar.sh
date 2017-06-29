#!/bin/bash
clang -DNDEBUG -I.. gen_cbar.c -o gen_cbar -L../.. -lvn -lm
./gen_cbar -6.60000000000000000E+01 5.84962500721172063E-01 32 496 1024 plt8/jet8.txt cbar_ex_32.bmp lg

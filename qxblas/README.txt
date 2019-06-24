How to build:

make [WP=4|8|10|16] [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
(GNU make on *nix, or)
nmake.exe [WP=4|8|16] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
(MS make on Windows)

Output:

../qx_wp.fi
../libqxblas.a
(*nix, or)
..\qx_wp.fi
..\qxblas.lib
(Windows)

Variables:

CPU=x64
Intel CPUs with IFORT.
The only option for Windows, for now.

CPU=x200
Intel Xeon Phi (KNL) with IFORT.

CPU=gnu (default if not defined)
GNU Fortran; the only option to have WP=10.

Precision:

WP=4: single
WP=8: double
WP=10: Intel 80-bit extended
WP=16: quad
or any other available KIND parameter.

q-routines: REAL(KIND=WP)
x-routines: COMPLEX(KIND=WP)

The naming is chosen to match the OpenBLAS extended-precision routines.

Some LAPACK routines are included, as well.

TODO:

xerbla.f90 and lsame.f90 not included in the build, since they are
available from other BLAS libraries; include them in the appropriate
makefile, if needed.

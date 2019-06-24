# JACSD
Some utilities for the Jacobi-type (G)SVD algorithms and beyond, plus a Cosine-Sine Decomposition tester.

Available here (for now):
* the shared-memory vectorized Jacobi-type SVD,
* a multi-precision BLAS library (qxblas),
* a Jacobi strategies library (jstrat),
* a utility library (vn),
* and some testing code for LAPACK CS.

## Building

### Prerequisites

A recent 64-bit Linux (e.g., CentOS 7.6) or macOS (e.g., Mojave) is needed.

Have the Intel MKL (Math Kernel Library) installed.

### Make options

Run ``make`` as follows:
```bash
make [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
```
where ``CPU`` should be set for the Intel C++ and Fortran compilers to ``x64`` for Xeons, or to ``x200`` for Xeon Phi KNLs, respectively.
If ``CPU`` is not set, GNU C (Clang on macOS) and Fortran compilers will be used instead.
GNU Fortran 9 is *not* supported!

Here, ``NDEBUG`` should be set to the desired optimization level (``3`` is a sensible choice).
If unset, the predefined debug-mode build options will be used.

For example, ``make CPU=x200 NDEBUG=3 clean all`` will trigger a full, release-mode rebuild for the KNLs.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).

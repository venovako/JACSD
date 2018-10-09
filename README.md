# JACSD
Cosine-Sine Decomposition based on the Jacobi SVD.

Available here (for now):
* the shared-memory vectorized Jacobi-type SVD,
* a multi-precision BLAS library (qxblas),
* a Jacobi strategies library (jstrat),
* a utility library (vn),
* and some testing code for LAPACK CS.

## Building

### Prerequisites

A recent 64-bit Linux (e.g., CentOS 7.5) or macOS (e.g., High Sierra) is needed.

Have the Intel MKL (Math Kernel Library) installed.

### Make options

Run ``make`` as follows:
```bash
make [CPU=x64|x200] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
```
where ``CPU`` should be set for the Intel C++ and Fortran compilers to ``x64`` for Xeons (*not* tested recently), or to ``x200`` for Xeon Phi KNLs, respectively.
If ``CPU`` is not set, GNU C and Fortran compilers will be used instead.

Here, ``NDEBUG`` should be set to the desired optimization level (``3`` is a sensible choice).
If unset, the predefined debug-mode build options will be used.

For example, ``make CPU=x200 NDEBUG=3 clean all`` will trigger a full, release-mode rebuild for the KNLs.

This work has been supported in part by Croatian Science Foundation under the project IP--2014--09--3670.

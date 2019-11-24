# JACSD
Some utilities for the Jacobi-type (G/H)SVD algorithms and beyond, plus a Cosine-Sine Decomposition tester.

Available here (for now):
* a shared-memory vectorized Jacobi-type SVD,
* a multi-precision BLAS library (qxblas),
* a GSVD test data generator (tgengsvd) \[2\],
* a HSVD test data generator (tgenhsvd),
* a Jacobi strategies library (jstrat) \[1,2\],
* a utility library (vn) \[1,2\],
* and some testing code for the LAPACK CS.

\[1\] These subdirectories are also a part of the supplementary material for or otherwise related to the paper arXiv:[1907.08560](https://arxiv.org/abs/1907.08560) \[math.NA\].

\[2\] These subdirectories are also a part of the supplementary material for or otherwise related to the paper arXiv:[1909.00101](https://arxiv.org/abs/1909.00101) \[math.NA\].

## Building

### Prerequisites

A recent 64-bit Linux (e.g., CentOS 7.7 with devtoolset-8) or macOS (e.g., Catalina) is needed.

Have the Intel MKL (Math Kernel Library) installed.

### Make options

Run ``make`` as follows:
```bash
make [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
```
where ``CPU`` should be set for the Intel C/C++ and Fortran compilers to ``x64`` for Xeons, or to ``x200`` for Xeon Phi KNLs, respectively.
If ``CPU`` is not set, GNU C/C++/Fortran compilers will be used instead.

GNU Fortran 9 is *not* supported (though it might work)!
Please take a look [here](https://gcc.gnu.org/gcc-9/changes.html) for the explanation regarding the MAX and MIN intrinsics.
Currently, only GPU Fortran *8* is fully supported.
On RHEL/CentOS it is provided by, e.g., devtoolset-8.

Here, ``NDEBUG`` should be set to the desired optimization level (``3`` is a sensible choice).
If unset, the predefined debug-mode build options will be used.

For example, ``make CPU=x200 NDEBUG=3 clean all`` will trigger a full, release-mode rebuild for the KNLs.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).

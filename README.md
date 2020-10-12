# JACSD
Some utilities for the Jacobi-type (G/H)SVD algorithms and beyond, plus a Cosine-Sine Decomposition tester.

Available here (for now):
* a shared-memory vectorized Jacobi-type SVD,
* a multi-precision BLAS library (qxblas),
* an orthogonality checker (tortho),
* a GSVD test data generator (tgengsvd) \[2\],
* a HSVD test data generator (tgenhsvd) \[3\],
* a skew-symmetric/Hermitian test data generator (tgenskew) \[4\],
* a Jacobi strategies library (jstrat) \[1,2\],
* a utility library (vn) \[1,2,3\],
* and some testing code for the LAPACK CS.

\[1\] These subdirectories are also a part of the supplementary material for or otherwise related to the paper [doi:10.1137/19M1277813](https://doi.org/10.1137/19M1277813 "The LAPW Method with Eigendecomposition Based on the Hari–Zimmermann Generalized Hyperbolic SVD") with the preprint at arXiv:[1907.08560](https://arxiv.org/abs/1907.08560 "The LAPW method with eigendecomposition based on the Hari–Zimmermann generalized hyperbolic SVD") \[math.NA\].

\[2\] These subdirectories are also a part of the supplementary material for or otherwise related to the paper arXiv:[1909.00101](https://arxiv.org/abs/1909.00101 "An implicit Hari–Zimmermann algorithm for the generalized SVD on the GPUs") \[math.NA\].

\[3\] These subdirectories are also a part of the supplementary material for or otherwise related to the paper arXiv:[2003.06701](https://arxiv.org/abs/2003.06701 "A Kogbetliantz-type algorithm for the hyperbolic SVD") \[math.NA\].

\[4\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper [doi:10.1016/j.amc.2020.125263](https://doi.org/10.1016/j.amc.2020.125263 "The antitriangular factorization of skew-symmetric matrices") with the preprint at arXiv:[1909.00092](https://arxiv.org/abs/1909.00092 "The antitriangular factorization of skew-symmetric matrices") \[math.NA\].

## Building

### Prerequisites

A recent 64-bit Linux (e.g., CentOS 7.8 with devtoolset-8) or macOS (e.g., Catalina) is needed.

Have the Intel MKL (Math Kernel Library) installed.

### Make options

Run ``make`` as follows:
```bash
make [COMPILER=gnu|x64|x200|nvidia] [NDEBUG=0|1|2|3|4|5] [all|clean|help]
```
where ``COMPILER`` should be set for the Intel C/C++ and Fortran compilers (version 19.1+/2020+ recommended) to ``x64`` for Xeons, or to ``x200`` for Xeon Phi KNLs, respectively.
If ``COMPILER`` is not set, GNU C/C++/Fortran compilers will be used instead.

GNU Fortran 9 and 10 are *not* supported (though they might work)!
Please take a look [here](https://gcc.gnu.org/gcc-9/changes.html) for the explanation regarding the MAX and MIN intrinsics.
Currently, only GPU Fortran *8* is fully supported.
On RHEL/CentOS it is provided by, e.g., devtoolset-8.

Here, ``NDEBUG`` should be set to the desired optimization level (``3`` is a sensible choice).
If unset, the predefined debug-mode build options will be used.

For example, ``make COMPILER=x200 NDEBUG=3 clean all`` will trigger a full, release-mode rebuild for the KNLs.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).

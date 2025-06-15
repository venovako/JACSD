# JACSD
Some utilities for the Jacobi-type (G/H)SVD algorithms and beyond, plus a Cosine-Sine Decomposition tester.

Available here (for now):
* a multi-precision BLAS library (qxblas) \[1,2,5\],
* an orthogonality checker (tortho) \[1,2,5\],
* an SVD test data generator (tgensvd) \[5\],
* a GSVD test data generator (tgengsvd) \[2\],
* a HSVD test data generator (tgenhsvd) \[3\],
* a skew-symmetric/Hermitian test data generator (tgenskew) \[4\],
* a Jacobi strategies library (jstrat) \[1,2,5\],
* a symmetric/Hermitian EVD test data generator (tgenevd) \[6\],
* a utility library (vn) \[1,2,3\],
* the first attempt of a shared-memory vectorized Jacobi-type SVD (see [VecJac](https://github.com/venovako/VecJac) repository for a better version),
* and some testing code for the LAPACK CS (src).

\[1\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper doi:[10.1137/19M1277813](https://doi.org/10.1137/19M1277813 "The LAPW Method with Eigendecomposition Based on the Hari–Zimmermann Generalized Hyperbolic SVD") with the preprint at arXiv:[1907.08560](https://arxiv.org/abs/1907.08560 "The LAPW method with eigendecomposition based on the Hari–Zimmermann generalized hyperbolic SVD") \[math.NA\].

\[2\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper doi:[10.1177/1094342020972772](https://doi.org/10.1177/1094342020972772 "Implicit Hari–Zimmermann algorithm for the generalized SVD on the GPUs") with the preprint at arXiv:[1909.00101](https://arxiv.org/abs/1909.00101 "An implicit Hari–Zimmermann algorithm for the generalized SVD on the GPUs") \[math.NA\].

\[3\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper doi:[10.1007/s11075-021-01197-4](https://doi.org/10.1007/s11075-021-01197-4 "A Kogbetliantz-type algorithm for the hyperbolic SVD") with the preprint at arXiv:[2003.06701](https://arxiv.org/abs/2003.06701 "A Kogbetliantz-type algorithm for the hyperbolic SVD") \[math.NA\].

\[4\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper doi:[10.1016/j.amc.2020.125263](https://doi.org/10.1016/j.amc.2020.125263 "The antitriangular factorization of skew-symmetric matrices") with the preprint at arXiv:[1909.00092](https://arxiv.org/abs/1909.00092 "The antitriangular factorization of skew-symmetric matrices") \[math.NA\].

\[5\] This subdirectory is also a part of the supplementary material for or otherwise related to the paper doi:[10.1137/22M1478847](https://doi.org/10.1137/22M1478847 "Vectorization of a thread-parallel Jacobi singular value decomposition method") with the preprint at arXiv:[2202.08361](https://arxiv.org/abs/2202.08361 "Vectorization of the Jacobi-type singular value decomposition method") \[math.NA\].

\[6\] This subdirectory is also related to the paper doi:[10.1016/j.cam.2024.116003](https://doi.org/10.1016/j.cam.2024.116003 "Accurate complex Jacobi rotations").

## Building

### Prerequisites

A recent 64-bit Linux (e.g., CentOS 7.9 with devtoolset-8) or macOS (e.g., Big Sur) is needed.

### Make options

Run ``make`` as follows:
```bash
make [COMPILER=x64x|x200|x64|gnu] [MARCH=...] [NDEBUG=optimization_level] [ABI=ilp64|lp64] [WP=...] [all|clean|help]
```

GNU Fortran versions 9 and above are *not* supported (though they might work)!
Please take a look [here](https://gcc.gnu.org/gcc-9/changes.html) for the explanation regarding the MAX and MIN intrinsics.
Currently, only GNU Fortran *8* is fully supported with ``COMPILER=gnu``.
On RHEL/CentOS 7 it is provided by, e.g., devtoolset-8.

By default, ``ABI=ilp64``, meaning that for Fortran it is assumed that ``INTEGER`` type is 8-byte-wide.
The more common ``ABI=lp64`` is *not* tested and may not work.

Here, ``NDEBUG`` should be set to the desired optimization level (``3`` is a sensible choice).
If unset, the predefined debug-mode build options will be used.

For example, ``make COMPILER=x200 NDEBUG=3 clean all`` will trigger a full, release-mode rebuild for the KNLs.

Static, single-threaded Windows executables can be found [here](https://web.math.pmf.unizg.hr/~venovako/venovako.exe) or [here](https://venovako.eu/venovako.exe).

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).

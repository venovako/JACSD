# QX BLAS

BLAS and more in extra precisions.

## Building

GNU make on Unices:
```
gmake [WP=10|16] [COMPILER=x64x|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]
```
MS make on Windows:
```
nmake.exe [NDEBUG=0|1|2|3|4|5] [ABI=lp64|ilp64] [all|clean|help]
```

### Precision

`WP` can be set to:
- `10`: Intel 80-bit extended (only with GNU Fortran)
- `16`: quad

or to any other `KIND` parameter available.

## Output

On Unices, ``../libqxblas$(WP)$(ABI)$(DEBUG).a``.

On Windows, ``..\qxblas$(WP)$(ABI)$(DEBUG).lib``.

`$(WP)` and `$(ABI)` are as above, and `$(DEBUG)` is either an empty string for release (`NDEBUG`) builds, or `g` (Unices) or `d` (Windows) otherwise.

## Usage

- `q`-routines: ``REAL(KIND=WP)``
- `x`-routines: ``COMPLEX(KIND=WP)``

The naming is chosen to match the [OpenBLAS](https://www.openblas.net) extended-precision routines.

Some [LAPACK](https://github.com/Reference-LAPACK) routines are included, as well.

## TODO

``xerbla.F90`` and ``lsame.F90`` are not included in the build, since they are available from other BLAS libraries.
Include them in the appropriate makefile, if needed.

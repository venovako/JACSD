#ifndef VN_LIB_H
#define VN_LIB_H

#ifdef __ICC
#include <mathimf.h>
#else /* !__ICC */
#ifdef __cplusplus
#include <complex>
#ifndef __CUDACC__
#include <math>
#endif /* !__CUDACC__ */
#else /* !__cplusplus */
#include <complex.h>
#include <math.h>
#endif /* __cplusplus */
#endif /* __ICC */

#ifdef __APPLE__
#include <alloca.h>
#else /* !__APPLE__ */
#ifdef _GNU_SOURCE
#include <alloca.h>
#include <malloc.h>
#endif /* _GNU_SOURCE */
#endif /* ?__APPLE__ */

#ifdef __cplusplus
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <climits>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctgmath>
#include <ctime>
#include <cwchar>
#else /* C11 */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fenv.h>
#include <float.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <time.h>
#include <wchar.h>
#endif /* __cplusplus */
#include <stdalign.h>
#include <execinfo.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#ifdef _OPENMP
#include <omp.h>
#endif /* _OPENMP */
/* for the KNLs */
#ifdef __AVX512PF__
#include <hbwmalloc.h>
#endif /* __AVX512PF__ */

#include "vn_stdc11.h"
#include "vn_attrs.h"
#include "vn_align.h"
#include "vn_assert.h"
#include "vn_error.h"
#include "vn_types.h"
#include "vn_simd.h"
#include "vn_variant.h"
#include "vn_alloc.h"
#include "vn_binio.h"
#include "vn_lock.h"
#include "vn_timer.h"

#include "vn_blas.h"
#include "vn_lapack.h"

#include "vn_bmp.h"
#include "vn_mtxvis.h"
#include "vn_cmplxvis.h"

#endif /* !VN_LIB_H */

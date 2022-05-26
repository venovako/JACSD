#ifndef VN_LIB_H
#define VN_LIB_H

// the latest language versions are assumed

#ifdef __ICC
#include <mathimf.h>
#else /* !__ICC */
#ifdef __cplusplus
#include <complex>
#include <cmath>
#else /* !__cplusplus */
#include <complex.h>
#include <math.h>
#endif /* ?__cplusplus */
#endif /* ?__ICC */

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
#include <cinttypes>
#include <climits>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cwchar>
#else /* !__cplusplus */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fenv.h>
#include <float.h>
#include <inttypes.h>
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
#endif /* ?__cplusplus */
#include <stdalign.h>
#include <execinfo.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <pthread.h>
#include <search.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#ifdef __APPLE__
#include <sys/sysctl.h>
#endif /* __APPLE__ */
#include <unistd.h>
#ifdef _OPENMP
#include <omp.h>
#endif /* _OPENMP */
// for the KNLs
#ifdef __AVX512PF__
#include <hbwmalloc.h>
#endif /* __AVX512PF__ */
#include <x86intrin.h>
#ifdef TSC_FREQ_HZ
#if (TSC_FREQ_HZ == 0ull)
#ifndef __ICC
#include <cpuid.h>
#endif /* !__ICC */
#endif /* ?TSC_FREQ_HZ */
#endif /* TSC_FREQ_HZ */

#include "vn_stdc11.h"
#include "vn_attrs.h"
#include "vn_types.h"
#include "vn_simd.h"
#include "vn_align.h"
#include "vn_assert.h"
#include "vn_error.h"
#include "vn_variant.h"
#include "vn_alloc.h"
#include "vn_binio.h"
#include "vn_lock.h"
#include "vn_timer.h"
#include "vn_profile.h"
#include "vn_blas.h"
#include "vn_lapack.h"
#include "vn_bmp.h"
#include "vn_mtxvis.h"
#include "vn_cmplxvis.h"

#endif /* !VN_LIB_H */

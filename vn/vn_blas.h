#ifndef VN_BLAS_H
#define VN_BLAS_H

#ifndef VN_NO_BLAS

#ifndef VN_LIB_H
#error vn_blas.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef VN_BLAS_R
#error VN_BLAS_R already defined
#endif /* VN_BLAS_R */

#ifdef VN_BLAS_C
#error VN_BLAS_C already defined
#endif /* VN_BLAS_C */

#if defined(USE_MKL)
#ifndef MKL_Complex8
#define MKL_Complex8 vn_complex_4
#endif /* !MKL_Complex8 */
#ifndef MKL_Complex16
#define MKL_Complex16 vn_complex_8
#endif /* !MKL_Complex16 */
#if (8 == VN_INTEGER_KIND)
#ifndef MKL_ILP64
#define MKL_ILP64
#endif /* !MKL_ILP64 */
#endif /* ?VN_INTEGER_KIND */
#include <mkl.h>
#if (4 == VN_REAL_KIND)
#define VN_BLAS_R(name) s##name
#define VN_BLAS_C(name) c##name
#elif (8 == VN_REAL_KIND)
#define VN_BLAS_R(name) d##name
#define VN_BLAS_C(name) z##name
#else /* unsupported */
#error BLAS(VN_REAL_KIND) not supported by Intel MKL
#endif /* ?VN_REAL_KIND */
#elif defined(USE_OPENBLAS)
#include "f77blas.h"
#if (4 == VN_REAL_KIND)
#define VN_BLAS_R(name) s##name##_
#define VN_BLAS_C(name) c##name##_
#elif (8 == VN_REAL_KIND)
#define VN_BLAS_R(name) d##name##_
#define VN_BLAS_C(name) z##name##_
#elif (10 <= VN_REAL_KIND)
#define VN_BLAS_R(name) q##name##_
#define VN_BLAS_C(name) x##name##_
#else /* unsupported */
#error BLAS(VN_REAL_KIND) not supported by OpenBLAS
#endif /* ?VN_REAL_KIND */
#else /* unsupported BLAS */
#error BLAS library not supported
#endif /* ?BLAS */

VN_EXTERN_C vn_integer vn_blas_prepare();
VN_EXTERN_C vn_integer vn_blas_finish(vn_integer *const curr, vn_integer *const nbuf);
VN_EXTERN_C vn_integer vn_blas_set_num_threads(const vn_integer nt);

#endif /* !VN_NO_BLAS */

#endif /* !VN_BLAS_H */

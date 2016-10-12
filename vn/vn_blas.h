#ifndef VN_BLAS_H
#define VN_BLAS_H

#ifndef VN_LIB_H
#error vn_blas.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef VN_BLAS_R
#error VN_BLAS_R already defined
#endif /* VN_BLAS_R */

#ifdef VN_BLAS_C
#error VN_BLAS_C already defined
#endif /* VN_BLAS_C */

#ifdef __ICC
#ifndef MKL_Complex8
#define MKL_Complex8 vn_complex_4
#else /* MKL_Complex8 */
#error MKL_Complex8 already defined
#endif /* !MKL_Complex8 */
#ifndef MKL_Complex16
#define MKL_Complex16 vn_complex_8
#else /* MKL_Complex16 */
#error MKL_Complex16 already defined
#endif /* !MKL_Complex16 */
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
#else /* !__ICC */
#ifdef USE_ESSL
#if (8 == VN_INTEGER_KIND)
#ifndef _ESV6464
#define _ESV6464
#endif /* !_ESV6464 */
#else /* 8 > VN_INTEGER_KIND */
#ifdef _ESV6464
#undef _ESV6464
#endif /* _ESV6464 */
#endif /* ?VN_INTEGER_KIND */
#include <essl.h>
#if (4 == VN_REAL_KIND)
#define VN_BLAS_R(name) s##name##_
#define VN_BLAS_C(name) c##name##_
#elif (8 == VN_REAL_KIND)
#define VN_BLAS_R(name) d##name##_
#define VN_BLAS_C(name) z##name##_
#else /* unsupported */
#error BLAS(VN_REAL_KIND) not supported by IBM ESSL
#endif /* ?VN_REAL_KIND */
VN_EXTERN_C void VN_BLAS_R(syrk)(const char *const UPLO, const char *const TRANS, const vn_integer *const N, const vn_integer *const K, const vn_real *const ALPHA, const vn_real *const A, const vn_integer *const LDA, const vn_real *const BETA, vn_real *const C, const vn_integer *const LDC);
#endif /* USE_ESSL */
#ifdef USE_OPENBLAS /* USE_OPENBLAS */
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
#endif /* USE_OPENBLAS */
#ifdef USE_ATLAS
#if (4 == VN_REAL_KIND)
#define VN_BLAS_R(name) s##name##_
#define VN_BLAS_C(name) c##name##_
#elif (8 == VN_REAL_KIND)
#define VN_BLAS_R(name) d##name##_
#define VN_BLAS_C(name) z##name##_
#else /* unsupported */
#error BLAS(VN_REAL_KIND) not supported by ATLAS
#endif /* ?VN_REAL_KIND */
VN_EXTERN_C void VN_BLAS_R(syrk)(const char *const UPLO, const char *const TRANS, const vn_integer *const N, const vn_integer *const K, const vn_real *const ALPHA, const vn_real *const A, const vn_integer *const LDA, const vn_real *const BETA, vn_real *const C, const vn_integer *const LDC);
#endif /* USE_ATLAS */
#endif /* ?__ICC */

#endif /* !VN_BLAS_H */

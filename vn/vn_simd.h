#ifndef VN_SIMD_H
#define VN_SIMD_H

#ifndef VN_LIB_H
#error vn_align.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_SIMD_BITS_MAX
#ifdef __MIC__
#define VN_SIMD_BITS_MAX 512
#else /* CPU */
#if defined(__AVX512F__)
#define VN_SIMD_BITS_MAX 512
#elif defined(__AVX__)
#define VN_SIMD_BITS_MAX 256
#elif defined(__SSE__)
#define VN_SIMD_BITS_MAX 128
#else /* POWER8 */
#define VN_SIMD_BITS_MAX 128
#endif /* ?CPU */
#endif /* __MIC__ */
#endif /* !VN_SIMD_BITS_MAX */

#ifndef VN_SIMD_BYTES_MAX
#define VN_SIMD_BYTES_MAX ((VN_SIMD_BITS_MAX) / (CHAR_BIT))
#else /* VN_SIMD_BYTES_MAX */
#error VN_SIMD_BYTES_MAX already defined
#endif /* !VN_SIMD_BYTES_MAX */

#ifndef VN_SIMD_BITS
#define VN_SIMD_BITS VN_SIMD_BITS_MAX
#else /* VN_SIMD_BITS */
#if (VN_SIMD_BITS > VN_SIMD_BITS_MAX)
#error VN_SIMD_BITS > VN_SIMD_BITS_MAX
#endif /* VN_SIMD_BITS > VN_SIMD_BITS_MAX */
#endif /* !VN_SIMD_BITS */

#ifndef VN_SIMD_BYTES
#define VN_SIMD_BYTES ((VN_SIMD_BITS) / (CHAR_BIT))
#else /* VN_SIMD_BYTES */
#error VN_SIMD_BYTES already defined
#endif /* !VN_SIMD_BYTES */

#ifndef VN_SIMD_LANES_I
#define VN_SIMD_LANES_I ((VN_SIMD_BYTES) / (VN_INTEGER_KIND))
#else /* VN_SIMD_LANES_I */
#error VN_SIMD_LANES_I already defined
#endif /* !VN_SIMD_LANES_I */

#ifndef VN_SIMD_LANES_R
#if (VN_REAL_KIND == 10)
/* no vectorization for extended, but if it were... */
#define VN_SIMD_LANES_R ((VN_SIMD_BYTES) / 16)
#else /* VN_REAL KIND != 10 */
#define VN_SIMD_LANES_R ((VN_SIMD_BYTES) / (VN_REAL_KIND))
#endif /* ?VN_REAL_KIND */
#else /* VN_SIMD_LANES_R */
#error VN_SIMD_LANES_R already defined
#endif /* !VN_SIMD_LANES_R */

#ifndef VN_SIMD_LANES_C
#if (VN_REAL_KIND == 10)
/* no vectorization for extended, but if it were... */
#define VN_SIMD_LANES_C ((VN_SIMD_BYTES) / 32)
#else /* VN_REAL KIND != 10 */
#define VN_SIMD_LANES_C ((VN_SIMD_BYTES) / (2 * (VN_REAL_KIND)))
#endif /* ?VN_REAL_KIND */
#else /* VN_SIMD_LANES_C */
#error VN_SIMD_LANES_C already defined
#endif /* !VN_SIMD_LANES_C */

#ifndef VN_L1D_CLS_B
#ifdef __SSE__
#define VN_L1D_CLS_B 64
#else /* POWER8 */
#define VN_L1D_CLS_B 128
#endif /* __SSE__ */
#error VN_L1D_CLS_B already defined
#endif /* !VN_L1D_CLS_B */

#ifndef VN_CL1_I
#define VN_CL1_I (VN_L1D_CLS_B / VN_INTEGER_KIND)
#else /* VN_CL1_I */
#error VN_CL1_I already defined
#endif /* !VN_CL1_I */

#ifndef VN_CL1_R
#if (VN_REAL_KIND == 10)
#define VN_CL1_R (VN_L1D_CLS_B / 16)
#else /* VN_REAL KIND != 10 */
#define VN_CL1_R (VN_L1D_CLS_B / VN_REAL_KIND)
#endif /* ?VN_REAL_KIND */
#else /* VN_CL1_R */
#error VN_CL1_R already defined
#endif /* !VN_CL1_R */

#ifndef VN_CL1_C
#if (VN_REAL_KIND == 10)
#define VN_CL1_C (VN_L1D_CLS_B / 32)
#else /* VN_REAL KIND != 10 */
#define VN_CL1_C (VN_L1D_CLS_B / (2 * VN_REAL_KIND))
#endif /* ?VN_REAL_KIND */
#else /* VN_CL1_C */
#error VN_CL1_C already defined
#endif /* !VN_CL1_C */

#ifndef VN_L1D_B
#ifdef __SSE__
#define VN_L1D_B 32768
#else /* POWER8 */
#define VN_L1D_B 65536
#endif /* __SSE__ */
#else /* VN_L1D_B */
#error VN_L1D_B already defined
#endif /* !VN_L1D_B */

/* SMT/hyperthreading */

#ifndef VN_MAX_TPC
#ifdef __MIC__
#define VN_MAX_TPC 4
#else /* CPU */
#if defined(__AVX512F__)
#define VN_MAX_TPC 4
#elif defined(__SSE__)
#define VN_MAX_TPC 2
#else /* POWER8 */
#define VN_MAX_TPC 8
#endif /* ?CPU */
#endif /* __MIC__ */
#else /* VN_MAX_TPC */
#error VN_MAX_TPC already defined
#endif /* !VN_MAX_TPC */

#ifndef VN_TPC
#define VN_TPC 1
#else /* VN_TPC */
#if (VN_TPC < 1) || (VN_TPC > VN_MAX_TPC)
#error VN_TPC invalid
#endif /* ?VN_TPC */
#endif /* !VN_TPC */

#endif /* !VN_SIMD_H */

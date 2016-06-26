#ifndef VN_SIMD_H
#define VN_SIMD_H

#ifndef VN_LIB_H
#error vn_align.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_SIMD_BITS_MAX
#ifdef __MIC__
#define VN_SIMD_BITS_MAX 512
#else /* CPU */
#if defined(__AVX__)
#define VN_SIMD_BITS_MAX 256
#elif defined(__SSE__)
#define VN_SIMD_BITS_MAX 128
#else /* default vector size */
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
#define VN_SIMD_LANES_R ((VN_SIMD_BYTES) / 16)
#else /* VN_REAL KIND != 10 */
#define VN_SIMD_LANES_R ((VN_SIMD_BYTES) / (VN_REAL_KIND))
#endif /* ?VN_REAL_KIND */
#else /* VN_SIMD_LANES_R */
#error VN_SIMD_LANES_R already defined
#endif /* !VN_SIMD_LANES_R */

#ifndef VN_SIMD_LANES_C
#if (VN_REAL_KIND == 10)
#define VN_SIMD_LANES_C ((VN_SIMD_BYTES) / 32)
#else /* VN_REAL KIND != 10 */
#define VN_SIMD_LANES_C ((VN_SIMD_BYTES) / (2 * (VN_REAL_KIND)))
#endif /* ?VN_REAL_KIND */
#else /* VN_SIMD_LANES_C */
#error VN_SIMD_LANES_C already defined
#endif /* !VN_SIMD_LANES_C */

#endif /* !VN_SIMD_H */

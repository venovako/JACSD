#ifndef VN_ALIGN_H
#define VN_ALIGN_H

#ifndef VN_LIB_H
#error vn_align.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_ALIGN_BYTES
#define VN_ALIGN_BYTES VN_SIMD_BYTES_MAX
#endif /* !VN_ALIGN_BYTES */

#ifndef VN_VAR_ALIGNED
#if defined(__GNUC__)
#define VN_VAR_ALIGNED __attribute__((aligned(VN_ALIGN_BYTES)))
#else /* __xlC__ */
#define VN_VAR_ALIGNED __attribute__((__aligned__(VN_ALIGN_BYTES)))
#endif /* __GNUC__ */
#else /* VN_VAR_ALIGNED */
#error VN_VAR_ALIGNED already defined
#endif /* !VN_VAR_ALIGNED */

#ifndef VN_IS_ALIGNED
#if (defined(__ICC) && defined(NDEBUG))
#define VN_IS_ALIGNED(a) __assume_aligned(a, VN_ALIGN_BYTES)
#else /* !__ICC || !NDEBUG */
#define VN_IS_ALIGNED(a) VN_ASSERT(!((intptr_t)(a) % VN_ALIGN_BYTES))
#endif /* __ICC && NDEBUG */
#else /* VN_IS_ALIGNED */
#error VN_IS_ALIGNED already defined
#endif /* !VN_IS_ALIGNED */

#ifndef VN_IS_ALIGNED2
#define VN_IS_ALIGNED2(A, ldA)                                          \
  VN_IS_ALIGNED(A);                                                     \
  VN_ASSERT(!((ldA) % (VN_ALIGN_BYTES / sizeof(*A))))
#else /* VN_IS_ALIGNED2 */
#error VN_IS_ALIGNED2 already defined
#endif /* !VN_IS_ALIGNED2 */

#endif /* !VN_ALIGN_H */

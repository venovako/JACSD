#ifndef VN_ALIGN_H
#define VN_ALIGN_H

#ifndef VN_LIB_H
#error vn_align.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_ALIGN_BYTES
#define VN_ALIGN_BYTES VN_L1D_CLS_B
#endif /* !VN_ALIGN_BYTES */

#ifndef VN_ASSUME_ALIGNED
#ifdef __ICC
#define VN_ASSUME_ALIGNED(a) __assume_aligned((a), (VN_ALIGN_BYTES))
#else /* !__ICC */
#if (defined(__GNUC__) || defined(__clang__))
#define VN_ASSUME_ALIGNED(a) (void)__builtin_assume_aligned((a), (VN_ALIGN_BYTES))
#else /* !__GNUC__ && !__clang__ */
#define VN_ASSUME_ALIGNED(a) (void)0
#endif /* ?(__GNUC__ || __clang__ ) */
#endif /* ?__ICC */
#else /* VN_ASSUME_ALIGNED */
#error VN_ASSUME_ALIGNED already defined
#endif /* ?VN_ASSUME_ALIGNED */

#ifndef VN_VAR_ALIGNED
#if (defined(__GNUC__) || defined(__clang__))
#define VN_VAR_ALIGNED __attribute__((aligned(VN_ALIGN_BYTES)))
#else /* __xlC__ */
#define VN_VAR_ALIGNED __attribute__((__aligned__(VN_ALIGN_BYTES)))
#endif /* ?(__GNUC__ || __clang__) */
#else /* VN_VAR_ALIGNED */
#error VN_VAR_ALIGNED already defined
#endif /* !VN_VAR_ALIGNED */

#ifndef VN_IS_ALIGNED
#ifdef NDEBUG
#ifdef __ICC
#define VN_IS_ALIGNED(a) __assume_aligned((a), (VN_ALIGN_BYTES))
#else /* !__ICC */
#if (defined(__GNUC__) || defined(__clang__))
#define VN_IS_ALIGNED(a) (void)__builtin_assume_aligned((a), (VN_ALIGN_BYTES))
#else /* !__GNUC__ && !__clang__ */
#define VN_IS_ALIGNED(a) VN_ASSERT(!((intptr_t)(a) % (VN_ALIGN_BYTES)))
#endif /* ?(__GNUC__ || __clang__) */
#endif /* ?__ICC */
#else /* !NDEBUG */
#define VN_IS_ALIGNED(a) VN_ASSERT(!((intptr_t)(a) % (VN_ALIGN_BYTES)))
#endif /* ?NDEBUG */
#else /* VN_IS_ALIGNED */
#error VN_IS_ALIGNED already defined
#endif /* ?VN_IS_ALIGNED */

#ifndef VN_IS_ALIGNED2
#define VN_IS_ALIGNED2(A, ldA)                            \
  VN_IS_ALIGNED(A);                                       \
  VN_ASSERT(!((ldA) % ((VN_ALIGN_BYTES) / sizeof(*(A)))))
#else /* VN_IS_ALIGNED2 */
#error VN_IS_ALIGNED2 already defined
#endif /* ?VN_IS_ALIGNED2 */

#endif /* !VN_ALIGN_H */

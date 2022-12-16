#ifndef VN_ALIGN_H
#define VN_ALIGN_H

#ifndef VN_LIB_H
#error vn_align.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_ALIGN_BYTES
#define VN_ALIGN_BYTES VN_L1D_CLS_B
#endif /* !VN_ALIGN_BYTES */

#ifndef VN_ASSUME_ALIGNED
#if (defined(__ICC) || defined(__INTEL_CLANG_COMPILER) || defined(__INTEL_LLVM_COMPILER))
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

#endif /* !VN_ALIGN_H */

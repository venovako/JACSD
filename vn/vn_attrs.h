#ifndef VN_ATTRS_H
#define VN_ATTRS_H

#ifndef VN_LIB_H
#error vn_attrs.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_EXTERN_C
#ifdef __cplusplus
#define VN_EXTERN_C extern "C"
#else /* C99 */
#define VN_EXTERN_C extern
#endif /* __cplusplus */
#else /* VN_EXTERN_C */
#error VN_EXTERN_C already defined
#endif /* !VN_EXTERN_C */

#ifndef VN_DEAD_CODE
#ifdef __GNUC__
#ifdef __ICC
#define VN_DEAD_CODE __assume(0)
#else /* !__ICC */
#define VN_DEAD_CODE __builtin_unreachable()
#endif /* __ICC */
#else /* !__GNUC__ */
#define VN_DEAD_CODE
#endif /* __GNUC__ */
#else /* VN_DEAD_CODE */
#error VN_DEAD_CODE already defined
#endif /* !VN_DEAD_CODE */

#ifndef VN_VAR_UNUSED
#ifdef __GNUC__
#define VN_VAR_UNUSED __attribute__((unused))
#else /* !__GNUC__ */
#define VN_VAR_UNUSED
#endif /* __GNUC__ */
#else /* VN_VAR_UNUSED */
#error VN_VAR_UNUSED already defined
#endif /* !VN_VAR_UNUSED */

#endif /* !VN_ATTRS_H */

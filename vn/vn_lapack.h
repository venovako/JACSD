#ifndef VN_LAPACK_H
#define VN_LAPACK_H

#ifndef VN_LIB_H
#error vn_lapack.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef VN_LAPACK_R
#error VN_LAPACK_R already defined
#endif /* VN_LAPACK_R */

#ifdef VN_LAPACK_C
#error VN_LAPACK_C already defined
#endif /* VN_LAPACK_C */

#if (4 == VN_REAL_KIND)
#define VN_LAPACK_R(name) s##name##_
#define VN_LAPACK_C(name) c##name##_
#elif (8 == VN_REAL_KIND)
#define VN_LAPACK_R(name) d##name##_
#define VN_LAPACK_C(name) z##name##_
#else /* unsupported */
#error LAPACK(VN_REAL_KIND) not supported
#endif /* ?VN_REAL_KIND */

#endif /* !VN_LAPACK_H */

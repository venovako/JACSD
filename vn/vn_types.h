#ifndef VN_TYPES_H
#define VN_TYPES_H

#ifndef VN_LIB_H
#error vn_types.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef __ICC
#include <mathimf.h>
#else /* !__ICC */
#ifdef __cplusplus
#include <complex>
#ifndef __CUDACC__
#include <math>
#endif /* !__CUDACC__ */
#else /* !__cplusplus */
#include <complex.h>
#include <math.h>
#endif /* __cplusplus */
#endif /* __ICC */

#ifdef __cplusplus
/* C headers */
#include <cfloat>
#include <climits>
#include <cstdint>
#include <cwchar>
#else /* C99 */
#include <tgmath.h>
#include <float.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <wchar.h>
#endif /* __cplusplus */

#ifndef __CUDACC__
static_assert(8 == CHAR_BIT, "vn_char_bit_8");
static_assert(1 == sizeof(char), "vn_char_sz_1");
static_assert(4 == sizeof(wchar_t), "vn_wchar_t_sz_4");
static_assert(4 == sizeof(float), "vn_float_sz_4");
static_assert(8 == sizeof(double), "vn_double_sz_8");
static_assert(16 == sizeof(long double), "vn_ldouble_sz_16");
#endif /* !__CUDACC__ */

#ifndef VN_CHARACTER_KIND
#define VN_CHARACTER_KIND 1
#endif /* VN_CHARACTER_KIND */

#ifdef MkChar
#error MkChar already defined
#endif /* MkChar */

#if (1 == VN_CHARACTER_KIND)
#define MkChar(x) x
typedef char vn_character;
#elif (4 == VN_CHARACTER_KIND)
#define MkChar(x) L##x
typedef wchar_t vn_character;
#else /* unsupported */
#error VN_CHARACTER_KIND must be one of { 1, 4 }
#endif /* ?VN_CHARACTER_KIND */

typedef int8_t vn_integer_1;
typedef int16_t vn_integer_2;
typedef int32_t vn_integer_4;
typedef int64_t vn_integer_8;

/* default integer is 64-bit signed */
#ifndef VN_INTEGER_KIND
#define VN_INTEGER_KIND 8
#endif /* !VN_INTEGER_KIND */

#ifdef MkInt
#error MkInt already defined
#endif /* MkInt */

#ifdef VN_INTEGER_MIN
#error VN_INTEGER_MIN already defined
#endif /* VN_INTEGER_MIN */

#ifdef VN_INTEGER_MAX
#error VN_INTEGER_MAX already defined
#endif /* VN_INTEGER_MAX */

#if (4 == VN_INTEGER_KIND)
#define MkInt INT32_C
typedef vn_integer_4 vn_integer;
#define VN_INTEGER_MIN INT32_MIN
#define VN_INTEGER_MAX INT32_MAX
#elif (8 == VN_INTEGER_KIND)
#define MkInt INT64_C
typedef vn_integer_8 vn_integer;
#define VN_INTEGER_MIN INT64_MIN
#define VN_INTEGER_MAX INT64_MAX
#else /* unsupported */
#error VN_INTEGER_KIND must be one of { 4, 8 }
#endif /* ?VN_INTEGER_KIND */

typedef vn_integer_1 vn_logical_1;
typedef vn_integer_2 vn_logical_2;
typedef vn_integer_4 vn_logical_4;
typedef vn_integer_8 vn_logical_8;

#ifndef VN_LOGICAL_KIND
#define VN_LOGICAL_KIND VN_INTEGER_KIND
#endif /* !VN_LOGICAL_KIND */

#ifdef MkBool
#error MkBool already defined
#endif /* MkBool */

#if (8 == VN_LOGICAL_KIND)
#define MkBool(x) INT64_C(x)
#else /* VN_LOGICAL_KIND < 8 */
#define MkBool(x) x
#endif /* ?VN_LOGICAL_KIND */

#if (1 == VN_LOGICAL_KIND)
typedef vn_logical_1 vn_logical;
#elif (2 == VN_LOGICAL_KIND)
typedef vn_logical_2 vn_logical;
#elif (4 == VN_LOGICAL_KIND)
typedef vn_logical_4 vn_logical;
#elif (8 == VN_LOGICAL_KIND)
typedef vn_logical_8 vn_logical;
#else /* unsupported */
#error VN_LOGICAL_KIND must be one of { 1, 2, 4, 8 }
#endif /* ?VN_LOGICAL_KIND */

#ifndef VN_FALSE
#define VN_FALSE MkBool(0)
#else /* VN_FALSE */
#error VN_FALSE already defined
#endif /* !VN_FALSE */

#ifndef VN_TRUE
#ifdef __ICC
#define VN_TRUE MkBool(-1)
#else /* !__ICC */
#define VN_TRUE MkBool(1)
#endif /* __ICC */
#endif /* !VN_TRUE */

typedef float vn_real_4;
typedef double vn_real_8;
/* TODO: FIXME for long doubles that are not Intel's 80-bit extended! */
typedef long double vn_real_10;

#ifdef __cplusplus
typedef std::complex<float> vn_complex_4;
typedef std::complex<double> vn_complex_8;
typedef std::complex<long double> vn_complex_10;
#else /* C99 */
typedef float complex vn_complex_4;
typedef double complex vn_complex_8;
typedef long double complex vn_complex_10;
#endif /* __cplusplus */

/* default real is double precision */
#ifndef VN_REAL_KIND
#define VN_REAL_KIND 8
#endif /* !VN_REAL_KIND */

#ifdef MkReal
#error MkReal already defined
#endif /* MkReal */

#ifdef MkCmplx
#error MkCmplx already defined
#endif /* MkCmplx */

#ifdef VN_REAL_EPS
#error VN_REAL_EPS already defined
#endif /* VN_REAL_EPS */

#ifdef VN_REAL_MIN
#error VN_REAL_MIN already defined
#endif /* VN_REAL_MIN */

#ifdef VN_REAL_MAX
#error VN_REAL_MAX already defined
#endif /* VN_REAL_MAX */

#ifdef VN_REAL_INF
#error VN_REAL_INF already defined
#endif /* VN_REAL_INF */

#ifdef VN_REAL_DIG
#error VN_REAL_DIG already defined
#endif /* VN_REAL_DIG */

#ifdef VN_REAL_EDG
#error VN_REAL_EDG already defined
#endif /* VN_REAL_EDG */

/* (1 + 1 + 1 + VN_REAL_DIG + 1 + 1 + VN_REAL_EDG) */
#ifdef VN_REAL_WID
#error VN_REAL_WID already defined
#endif /* VN_REAL_WID */

#ifdef VN_REAL_FMT
#error VN_REAL_FMT already defined
#endif /* VN_REAL_FMT */

#ifdef VN_REAL_FIN
#error VN_REAL_FIN already defined
#endif /* VN_REAL_FIN */

#if (4 == VN_REAL_KIND)
typedef vn_real_4 vn_real;
typedef struct {
  uint32_t mnt : 23;
  uint32_t ex2 : 8;
  uint32_t sgn : 1;
} vn_real_parts;
typedef union {
  vn_real whole;
  vn_real_parts parts;
  uint32_t bits;
} vn_real_view;
#define MkReal(x) x##F
#define VN_REAL_EPS FLT_EPSILON
#define VN_REAL_MIN FLT_MIN
#define VN_REAL_MAX FLT_MAX
#define VN_REAL_INF HUGE_VALF
#define VN_REAL_DIG 9
#define VN_REAL_EDG 2
#define VN_REAL_WID 16
#define VN_REAL_FMT "%# -16.9E"
#define VN_REAL_FIN "%E"
VN_EXTERN_C vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_4 payload);
VN_EXTERN_C vn_real MqNaN(const vn_integer_4 payload);
VN_EXTERN_C vn_real MsNaN(const vn_integer_4 payload);
typedef vn_complex_4 vn_complex;
#ifndef __cplusplus
#define MkCmplx(r,i) CMPLXF((r),(i))
#endif /* !__cplusplus */
#elif (8 == VN_REAL_KIND)
typedef vn_real_8 vn_real;
typedef struct {
  uint64_t mnt : 52;
  uint64_t ex2 : 11;
  uint64_t sgn : 1;
} vn_real_parts;
typedef union {
  vn_real whole;
  vn_real_parts parts;
  uint64_t bits;
} vn_real_view;
#define MkReal(x) x
#define VN_REAL_EPS DBL_EPSILON
#define VN_REAL_MIN DBL_MIN
#define VN_REAL_MAX DBL_MAX
#define VN_REAL_INF HUGE_VAL
#define VN_REAL_DIG 17
#define VN_REAL_EDG 3
#define VN_REAL_WID 25
#define VN_REAL_FMT "%# -25.17E"
#define VN_REAL_FIN "%lE"
VN_EXTERN_C vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_8 payload);
VN_EXTERN_C vn_real MqNaN(const vn_integer_8 payload);
VN_EXTERN_C vn_real MsNaN(const vn_integer_8 payload);
typedef vn_complex_8 vn_complex;
#ifndef __cplusplus
#define MkCmplx(r,i) CMPLX((r),(i))
#endif /* !__cplusplus */
#elif (10 == VN_REAL_KIND)
typedef vn_real_10 vn_real;
typedef struct {
  uint64_t mnt : 64;
  uint16_t ex2 : 15;
  uint16_t sgn : 1;
  uint8_t udef[6];
} vn_real_parts;
typedef union {
  vn_real whole;
  vn_real_parts parts;
  struct { uint64_t lo; uint16_t hi; uint8_t udef[6]; } bits;
} vn_real_view;
#define MkReal(x) x##L
#define VN_REAL_EPS LDBL_EPSILON
#define VN_REAL_MIN LDBL_MIN
#define VN_REAL_MAX LDBL_MAX
#define VN_REAL_INF HUGE_VALL
#define VN_REAL_DIG 21
#define VN_REAL_EDG 4
#define VN_REAL_WID 30
#define VN_REAL_FMT "%# -30.21LE"
#define VN_REAL_FIN "%LE"
VN_EXTERN_C vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_8 payload);
VN_EXTERN_C vn_real MqNaN(const vn_integer_8 payload);
VN_EXTERN_C vn_real MsNaN(const vn_integer_8 payload);
typedef vn_complex_10 vn_complex;
#ifndef __cplusplus
#define MkCmplx(r,i) CMPLXL((r),(i))
#endif /* !__cplusplus */
#else /* unsupported */
#error VN_REAL_KIND must be one of { 4, 8, 10 }
#endif /* ?VN_REAL_KIND */

#ifdef __cplusplus
#define MkCmplx(r,i) vn_complex((r),(i))
#endif /* !MkCmplx */

/*
  Adoi: macros for array offset calculation
  - d: number of array dimensions
  - o: memory order (C or Fortran)
  - i: 0-based or 1-based indexing
*/

#ifndef VN_A2C0
#define VN_A2C0(r, c, ldA) ((r) * (ldA) + (c))
#else /* VN_A2C0 */
#error VN_A2C0 already defined
#endif /* !VN_A2C0 */

#ifndef VN_A2C1
#define VN_A2C1(r, c, ldA) (((r)-MkInt(1)) * (ldA) + ((c)-MkInt(1)))
#else /* VN_A2C1 */
#error VN_A2C1 already defined
#endif /* !VN_A2C1 */

#ifndef VN_A2F0
#define VN_A2F0(r, c, ldA) ((c) * (ldA) + (r))
#else /* VN_A2F0 */
#error VN_A2F0 already defined
#endif /* !VN_A2F0 */

#ifndef VN_A2F1
#define VN_A2F1(r, c, ldA) (((c)-MkInt(1)) * (ldA) + ((r)-MkInt(1)))
#else /* VN_A2F1 */
#error VN_A2F1 already defined
#endif /* !VN_A2F1 */

#ifndef VN_A2C
#define VN_A2C VN_A2C0
#else /* VN_A2C */
#error VN_A2C already defined
#endif /* !VN_A2C */

#ifndef VN_A2F
#define VN_A2F VN_A2F0
#else /* VN_A2F */
#error VN_A2F already defined
#endif /* !VN_A2F */

/* default array order is column-major (Fortran) */
#ifndef MkIx2
#define MkIx2 VN_A2F
#endif /* !MkIx2 */

#endif /* VN_TYPES_H */

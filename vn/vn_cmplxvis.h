#ifndef VN_CMPLXVIS_H
#define VN_CMPLXVIS_H

#ifndef VN_LIB_H
#error vn_cmplxvis.h not intended for direct inclusion
#endif /* !VN_LIB_H */

typedef void (*vn_cmplxvis_op_ptr)(const vn_integer, const vn_integer, const vn_complex *const, const vn_integer, vn_complex *const, const vn_integer);
typedef vn_complex (*vn_cmplxvis_fn_ptr)(const vn_complex);

typedef struct {
  vn_integer cnt, act;
  vn_integer mA, nA;
  vn_integer mC, nC;
  vn_integer sx, sy;
  vn_complex small, large;
  vn_cmplxvis_op_ptr op;
  vn_cmplxvis_fn_ptr fn;
  vn_complex *C;
  vn_integer ldC;
  char fname[12];
  FILE *out;
} vn_cmplxvis_ctx;

#ifndef VN_CMPLXVIS_OP_A
#define VN_CMPLXVIS_OP_A MkInt(0)
#else /* VN_CMPLXVIS_OP_A */
#error VN_CMPLXVIS_OP_A already defined
#endif /* ?VN_CMPLXVIS_OP_A */

#ifndef VN_CMPLXVIS_OP_Ah
#define VN_CMPLXVIS_OP_Ah MkInt(1)
#else /* VN_CMPLXVIS_OP_Ah */
#error VN_CMPLXVIS_OP_Ah already defined
#endif /* ?VN_CMPLXVIS_OP_Ah */

#ifndef VN_NO_BLAS

#ifndef VN_CMPLXVIS_OP_AAh
#define VN_CMPLXVIS_OP_AAh MkInt(2)
#else /* VN_CMPLXVIS_OP_AAh */
#error VN_CMPLXVIS_OP_AAh already defined
#endif /* ?VN_CMPLXVIS_OP_AAh */

#ifndef VN_CMPLXVIS_OP_AhA
#define VN_CMPLXVIS_OP_AhA MkInt(3)
#else /* VN_CMPLXVIS_OP_AhA */
#error VN_CMPLXVIS_OP_AhA already defined
#endif /* ?VN_CMPLXVIS_OP_AhA */

#endif /* !VN_NO_BLAS */

#ifndef VN_CMPLXVIS_FN_Id
#define VN_CMPLXVIS_FN_Id MkInt(0)
#else /* VN_CMPLXVIS_FN_Id */
#error VN_CMPLXVIS_FN_Id already defined
#endif /* ?VN_CMPLXVIS_FN_Id */

#ifndef VN_CMPLXVIS_FN_Lg
#define VN_CMPLXVIS_FN_Lg MkInt(4)
#else /* VN_CMPLXVIS_FN_Lg */
#error VN_CMPLXVIS_FN_Lg already defined
#endif /* ?VN_CMPLXVIS_FN_Lg */

#ifndef VN_CMPLXVIS_FN_Ln
#define VN_CMPLXVIS_FN_Ln MkInt(8)
#else /* VN_CMPLXVIS_FN_Ln */
#error VN_CMPLXVIS_FN_Ln already defined
#endif /* ?VN_CMPLXVIS_FN_Ln */

#ifndef VN_CMPLXVIS_FN_Log
#define VN_CMPLXVIS_FN_Log MkInt(12)
#else /* VN_CMPLXVIS_FN_Log */
#error VN_CMPLXVIS_FN_Log already defined
#endif /* ?VN_CMPLXVIS_FN_Log */

#ifndef VN_CMPLXVIS_FN_Arg2PI
#define VN_CMPLXVIS_FN_Arg2PI MkInt(16)
#else /* VN_CMPLXVIS_FN_Arg2PI */
#error VN_CMPLXVIS_FN_Arg2PI already defined
#endif /* ?VN_CMPLXVIS_FN_Arg2PI */

VN_EXTERN_C vn_integer vn_cmplxvis_start(vn_cmplxvis_ctx **const ctx, const char *const fname, const vn_integer act, const vn_integer mA, const vn_integer nA, const vn_integer sx, const vn_integer sy, const vn_integer fname_len);
VN_EXTERN_C vn_integer vn_cmplxvis_frame(vn_cmplxvis_ctx *const ctx, const vn_complex *const A, const vn_integer ldA);
VN_EXTERN_C vn_integer vn_cmplxvis_stop(vn_cmplxvis_ctx *const ctx);

#endif /* !VN_CMPLXVIS_H */

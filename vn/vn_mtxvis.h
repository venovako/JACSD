#ifndef VN_MTXVIS_H
#define VN_MTXVIS_H

#ifndef VN_LIB_H
#error vn_mtxvis.h not intended for direct inclusion
#endif /* !VN_LIB_H */

typedef void (*vn_mtxvis_op_ptr)(const vn_integer, const vn_integer, const vn_real *const, const vn_integer, vn_real *const, const vn_integer);
typedef vn_real (*vn_mtxvis_fn_ptr)(const vn_real);

typedef struct {
  vn_integer cnt, act;
  vn_integer mA, nA;
  vn_integer mC, nC;
  vn_integer sx, sy;
  vn_real min_val, max_val;
  vn_mtxvis_op_ptr op;
  vn_mtxvis_fn_ptr fn;
  vn_real *C;
  vn_integer ldC;
  char fname[12];
  FILE *out;
} vn_mtxvis_ctx;

#ifndef VN_MTXVIS_OP_A
#define VN_MTXVIS_OP_A MkInt(0)
#else /* VN_MTXVIS_OP_A */
#error VN_MTXVIS_OP_A already defined
#endif /* !VN_MTXVIS_OP_A */

#ifndef VN_MTXVIS_OP_At
#define VN_MTXVIS_OP_At MkInt(1)
#else /* VN_MTXVIS_OP_At */
#error VN_MTXVIS_OP_At already defined
#endif /* !VN_MTXVIS_OP_At */

#ifndef VN_NO_BLAS

#ifndef VN_MTXVIS_OP_AAt
#define VN_MTXVIS_OP_AAt MkInt(2)
#else /* VN_MTXVIS_OP_AAt */
#error VN_MTXVIS_OP_AAt already defined
#endif /* !VN_MTXVIS_OP_AAt */

#ifndef VN_MTXVIS_OP_AtA
#define VN_MTXVIS_OP_AtA MkInt(3)
#else /* VN_MTXVIS_OP_AtA */
#error VN_MTXVIS_OP_AtA already defined
#endif /* !VN_MTXVIS_OP_AtA */

#endif /* !VN_NO_BLAS */

#ifndef VN_MTXVIS_FN_Id
#define VN_MTXVIS_FN_Id MkInt(0)
#else /* VN_MTXVIS_FN_Id */
#error VN_MTXVIS_FN_Id already defined
#endif /* !VN_MTXVIS_FN_Id */

#ifndef VN_MTXVIS_FN_Abs
#define VN_MTXVIS_FN_Abs MkInt(4)
#else /* VN_MTXVIS_FN_Abs */
#error VN_MTXVIS_FN_Abs already defined
#endif /* !VN_MTXVIS_FN_Abs */

#ifndef VN_MTXVIS_FN_Lg
#define VN_MTXVIS_FN_Lg MkInt(8)
#else /* VN_MTXVIS_FN_Lg */
#error VN_MTXVIS_FN_Lg already defined
#endif /* !VN_MTXVIS_FN_Lg */

#ifndef VN_MTXVIS_FN_Log
#define VN_MTXVIS_FN_Log MkInt(12)
#else /* VN_MTXVIS_FN_Log */
#error VN_MTXVIS_FN_Log already defined
#endif /* !VN_MTXVIS_FN_Log */

#ifndef VN_MTXVIS_FF_Bin
#define VN_MTXVIS_FF_Bin MkInt(0)
#else /* VN_MTXVIS_FF_Bin */
#error VN_MTXVIS_FF_Bin already defined
#endif /* !VN_MTXVIS_FF_Bin */

#ifndef VN_MTXVIS_FF_Asc
#define VN_MTXVIS_FF_Asc MkInt(16)
#else /* VN_MTXVIS_FF_Asc */
#error VN_MTXVIS_FF_Asc already defined
#endif /* !VN_MTXVIS_FF_Asc */

VN_EXTERN_C vn_integer vn_mtxvis_start(vn_mtxvis_ctx **const ctx, const char *const fname, const vn_integer act, const vn_integer mA, const vn_integer nA, const vn_integer sx, const vn_integer sy, const vn_integer fname_len);
VN_EXTERN_C vn_integer vn_mtxvis_frame(vn_mtxvis_ctx *const ctx, const vn_real *const A, const vn_integer ldA);
VN_EXTERN_C vn_integer vn_mtxvis_stop(vn_mtxvis_ctx *const ctx);

#endif /* !VN_MTXVIS_H */

#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
static void op_A(const vn_integer mA, const vn_integer nA, const vn_real *const A, const vn_integer ldA, vn_real *const C, const vn_integer ldC)
{
  for (vn_integer j = MkInt(0); j < nA; ++j)
    for (vn_integer i = MkInt(0); i < mA; ++i)
      C[MkIx2(i, j, ldC)] = A[MkIx2(i, j, ldA)];
}

static void op_At(const vn_integer mA, const vn_integer nA, const vn_real *const A, const vn_integer ldA, vn_real *const C, const vn_integer ldC)
{
  for (vn_integer j = MkInt(0); j < nA; ++j)
    for (vn_integer i = MkInt(0); i < mA; ++i)
      C[MkIx2(j, i, ldC)] = A[MkIx2(i, j, ldA)];
}

#ifndef VN_NO_BLAS

static void op_AAt(const vn_integer mA, const vn_integer nA, const vn_real *const A, const vn_integer ldA, vn_real *const C, const vn_integer ldC)
{
  static const vn_real one = MkReal(+1.0);
  static const vn_real zero = MkReal(+0.0);

  VN_BLAS_R(syrk)("L", "N", (vn_integer*)&mA, (vn_integer*)&nA, (vn_real*)&one, (vn_real*)A, (vn_integer*)&ldA, (vn_real*)&zero, C, (vn_integer*)&ldC);

  const vn_integer n = mA;
  const vn_integer n_1 = n - MkInt(1);
  for (vn_integer j = MkInt(0); j < n_1; ++j)
    for (vn_integer i = j + MkInt(1); i < n; ++i)
      C[MkIx2(j, i, ldC)] = C[MkIx2(i, j, ldC)];
}

static void op_AtA(const vn_integer mA, const vn_integer nA, const vn_real *const A, const vn_integer ldA, vn_real *const C, const vn_integer ldC)
{
  static const vn_real one = MkReal(+1.0);
  static const vn_real zero = MkReal(+0.0);

  VN_BLAS_R(syrk)("L", "T", (vn_integer*)&nA, (vn_integer*)&mA, (vn_real*)&one, (vn_real*)A, (vn_integer*)&ldA, (vn_real*)&zero, C, (vn_integer*)&ldC);

  const vn_integer n = nA;
  const vn_integer n_1 = n - MkInt(1);
  for (vn_integer j = MkInt(0); j < n_1; ++j)
    for (vn_integer i = j + MkInt(1); i < n; ++i)
      C[MkIx2(j, i, ldC)] = C[MkIx2(i, j, ldC)];
}

#endif /* !VN_NO_BLAS */

static vn_real fn_id(const vn_real x)
{
  return x;
}

static vn_real fn_abs(const vn_real x)
{
  return fabs(x);
}

static vn_real fn_lg(const vn_real x)
{
  const vn_real X = fabs(x);
  return ((X < VN_REAL_MIN) ? -VN_REAL_INF : log2(X));
}

static vn_real fn_log(const vn_real x)
{
  const vn_real X = fabs(x);
  return ((X < VN_REAL_MIN) ? -VN_REAL_INF : log10(X));
}

static void apply_op(vn_mtxvis_ctx *const ctx, const vn_real *const A, const vn_integer ldA)
{
  (ctx->op)(ctx->mA, ctx->nA, A, ldA, ctx->C, ctx->ldC);
}

static void apply_fn(vn_mtxvis_ctx *const ctx)
{
  for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
    for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
      vn_real *const x = ctx->C + MkIx2(i, j, ctx->ldC);
      *x = (ctx->fn)(*x);
      if (isfinite(*x)) {
        if (*x < ctx->min_val)
          ctx->min_val = *x;
        if (*x > ctx->max_val)
          ctx->max_val = *x;
      }
    }
  }
}

static void dump_bin(vn_mtxvis_ctx *const ctx)
{
  for (vn_integer i = MkInt(0); i < ctx->mC; ++i)
    for (vn_integer j = MkInt(0); j < ctx->nC; ++j)
      VN_SYSI_CALL(fwrite(&((ctx->C)[MkIx2(i, j, ctx->ldC)]), sizeof(vn_real), (size_t)1u, ctx->out) != (size_t)1u);
}

static void dump_asc(vn_mtxvis_ctx *const ctx)
{
  for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
    for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
      if (j)
        VN_SYSI_CALL(fprintf(ctx->out, " ") != 1);
      VN_SYSI_CALL(fprintf(ctx->out, VN_REAL_FMT, (ctx->C)[MkIx2(i, j, ctx->ldC)]) != VN_REAL_WID);
    }
    VN_SYSI_CALL(fprintf(ctx->out, "\n") != 1)
  }
}

vn_integer vn_mtxvis_start(vn_mtxvis_ctx **const ctx, const char *const fname, const vn_integer act, const vn_integer mA, const vn_integer nA, const vn_integer sx, const vn_integer sy, const vn_integer fname_len)
{
  if (!ctx)
    return MkInt(-1);
  if (!(fname && *fname))
    return MkInt(-2);
  if ((act < MkInt(0)) || (act >= MkInt(32)))
    return MkInt(-3);
  if (mA <= MkInt(0))
    return MkInt(-4);
  if (nA <= MkInt(0))
    return MkInt(-5);
  if (sx <= MkInt(0))
    return MkInt(-6);
  if (sy <= MkInt(0))
    return MkInt(-7);
  if (fname_len <= MkInt(0))
    return MkInt(-8);

  VN_SYSP_CALL(*ctx = (vn_mtxvis_ctx*)calloc((size_t)1u, sizeof(vn_mtxvis_ctx)));

  (*ctx)->act = act;
  (*ctx)->mA = mA;
  (*ctx)->nA = nA;

  (*ctx)->sx = sx;
  (*ctx)->sy = sy;

  (*ctx)->min_val = +VN_REAL_INF;
  (*ctx)->max_val = -VN_REAL_INF;

  switch ((*ctx)->act & MkInt(3)) {
  case VN_MTXVIS_OP_A:
    (*ctx)->mC = (*ctx)->mA;
    (*ctx)->nC = (*ctx)->nA;
    (*ctx)->op = op_A;
    break;
  case VN_MTXVIS_OP_At:
    (*ctx)->mC = (*ctx)->nA;
    (*ctx)->nC = (*ctx)->mA;
    (*ctx)->op = op_At;
    break;
#ifndef VN_NO_BLAS
  case VN_MTXVIS_OP_AAt:
    (*ctx)->mC = (*ctx)->mA;
    (*ctx)->nC = (*ctx)->mA;
    (*ctx)->op = op_AAt;
    break;
  case VN_MTXVIS_OP_AtA:
    (*ctx)->mC = (*ctx)->nA;
    (*ctx)->nC = (*ctx)->nA;
    (*ctx)->op = op_AtA;
    break;
#endif /* !VN_NO_BLAS */
  default:
    VN_DEAD_CODE;
  }

  switch ((*ctx)->act & MkInt(12)) {
  case VN_MTXVIS_FN_Id:
    (*ctx)->fn = fn_id;
    break;
  case VN_MTXVIS_FN_Abs:
    (*ctx)->fn = fn_abs;
    break;
  case VN_MTXVIS_FN_Lg:
    (*ctx)->fn = fn_lg;
    break;
  case VN_MTXVIS_FN_Log:
    (*ctx)->fn = fn_log;
    break;
  default:
    VN_DEAD_CODE;
  }

  /* allocate C, set ldC */
  VN_SYSP_CALL((*ctx)->C = VN_ALLOC2(vn_real, (*ctx)->mC, (*ctx)->nC, &((*ctx)->ldC), MkInt(-1)));

  const vn_integer asc = ((*ctx)->act) & MkInt(16);
  const vn_integer fnl = ((fname_len > MkInt(7)) ? MkInt(7) : fname_len);
  vn_integer i = MkInt(0);
  for (; i < fnl; ++i)
    ((*ctx)->fname)[i] = fname[i];
  ((*ctx)->fname)[i++] = '.';
  if (asc) {
    ((*ctx)->fname)[i++] = 't';
    ((*ctx)->fname)[i++] = 'x';
    ((*ctx)->fname)[i++] = 't';
  }
  else {
    ((*ctx)->fname)[i++] = 'd';
    ((*ctx)->fname)[i++] = 'a';
    ((*ctx)->fname)[i++] = 't';
  }

  VN_SYSP_CALL((*ctx)->out = fopen((*ctx)->fname, (asc ? "w+" : "w+b")));

  const size_t len = 128u * sizeof(char);
  char *const buf = (char*)alloca(len);
  /* set first 128 B of ctx->out to newlines */
  VN_SYSI_CALL(fwrite(memset(buf, (int)'\n', len), sizeof(char), len, (*ctx)->out) != len);

  return MkInt(0);
}

vn_integer vn_mtxvis_frame(vn_mtxvis_ctx *const ctx, const vn_real *const A, const vn_integer ldA)
{
  if (!ctx)
    return MkInt(-1);
  if (!A)
    return MkInt(-2);
  if (ldA <= MkInt(0))
    return MkInt(-3);

  apply_op(ctx, A, ldA);
  apply_fn(ctx);

  ((ctx->act & VN_MTXVIS_FF_Asc) ? dump_asc(ctx) : dump_bin(ctx));
  ++(ctx->cnt);

  return MkInt(0);
}

static inline uint32_t normalise(const vn_real x, const vn_real minv, const vn_real wid)
{
  if (x == -VN_REAL_INF)
    return 0u;
  if (!isfinite(x)) /* +Inf || NaN */
    return 255u;
  /* const vn_real y = ((x - minv) / wid) * MkReal(253.0) + MkReal(1.0); */
  const vn_real y = fma((x - minv) / wid, MkReal(253.0), MkReal(1.0));
  return (uint32_t)lround(y);
}

vn_integer vn_mtxvis_stop(vn_mtxvis_ctx *const ctx)
{
  if (!ctx)
    return MkInt(-1);

  vn_freep((const void**)&(ctx->C));

  rewind(ctx->out);
  int tot = 0, ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "cnt: %10u\n", (unsigned)(ctx->cnt))) != 16)
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "act: %2X\n", (unsigned)(ctx->act))) != 8);
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "< %5u x %5u\n", (unsigned)(ctx->mA), (unsigned)(ctx->nA))) != 16);
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "> %5u x %5u\n", (unsigned)(ctx->mC), (unsigned)(ctx->nC))) != 16);
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "min: " VN_REAL_FMT "\n", ctx->min_val)) != (VN_REAL_WID + 6));
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "MAX: " VN_REAL_FMT "\n", ctx->max_val)) != (VN_REAL_WID + 6));
  tot += ret;

  if (tot > 128)
    return MkInt(1);
  if (tot < 128)
    VN_SYSI_CALL(fseek(ctx->out, 128L, SEEK_SET));
  VN_SYSI_CALL(fflush(ctx->out));

  vn_integer idot = (vn_integer)strlen(ctx->fname) - MkInt(1);
  for (; idot >= MkInt(0); --idot)
    if ((ctx->fname)[idot] == '.')
      break;
  if (idot < MkInt(0))
    return MkInt(2);

  char *const cmap = (char*)alloca((idot + MkInt(5)) * sizeof(char));
  (void)strncpy(cmap, ctx->fname, (idot + MkInt(1)) * sizeof(char));
  cmap[idot + MkInt(1)] = 'p';
  cmap[idot + MkInt(2)] = 'l';
  cmap[idot + MkInt(3)] = 't';
  cmap[idot + MkInt(4)] = '\0';

  char *const bmpn = (char*)alloca((idot + MkInt(16)) * sizeof(char));
  (void)strncpy(bmpn, ctx->fname, idot * sizeof(char));

  vn_bmp_t bmp = (vn_bmp_t)NULL;
  if (vn_bmp_create(&bmp, (uint32_t)(ctx->nC * ctx->sx), (int32_t)(ctx->mC * -(ctx->sy)), (uint16_t)8u))
    return MkInt(3);
  if (vn_bmp_read_cmap(bmp, cmap))
    return MkInt(4);
  const vn_bmp_pixel_setter_t ps = vn_bmp_get_pixel_setter(bmp);
  if (!ps)
    return MkInt(5);

  const vn_real wid = ctx->max_val - ctx->min_val;
  if (wid == MkReal(0.0)) {
    for (vn_integer k = MkInt(0); k < ctx->cnt; ++k) {
      for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
        const uint32_t isy = (uint32_t)i * (uint32_t)(ctx->sy);
        for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
          const uint32_t jsx = (uint32_t)j * (uint32_t)(ctx->sx);
          for (vn_integer ii = MkInt(0); ii < ctx->sy; ++ii)
            for (vn_integer jj = MkInt(0); jj < ctx->sx; ++jj)
              /* mid-value for a constant image */
              ps(bmp, jsx + (uint32_t)jj, isy + (uint32_t)ii, 127u);
        }
      }
      (void)sprintf(bmpn + idot, "-%010u.bmp", (uint32_t)k);
      if (vn_bmp_fwrite(bmp, bmpn))
        return MkInt(6);
    }
  }
  else {
    vn_real x;
    if (ctx->act & VN_MTXVIS_FF_Asc) {
      for (vn_integer k = MkInt(0); k < ctx->cnt; ++k) {
        for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
          const uint32_t isy = (uint32_t)i * (uint32_t)(ctx->sy);
          for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
            const uint32_t jsx = (uint32_t)j * (uint32_t)(ctx->sx);
            VN_SYSI_CALL(fscanf(ctx->out, " " VN_REAL_FIN, &x) != 1);
            const uint32_t c = normalise(x, ctx->min_val, wid);
            for (vn_integer ii = MkInt(0); ii < ctx->sy; ++ii)
              for (vn_integer jj = MkInt(0); jj < ctx->sx; ++jj)
                ps(bmp, jsx + (uint32_t)jj, isy + (uint32_t)ii, c);
          }
        }
        (void)sprintf(bmpn + idot, "-%010u.bmp", (uint32_t)k);
        if (vn_bmp_fwrite(bmp, bmpn))
          return MkInt(6);
      }
    }
    else {
      for (vn_integer k = MkInt(0); k < ctx->cnt; ++k) {
        for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
          const uint32_t isy = (uint32_t)i * (uint32_t)(ctx->sy);
          for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
            const uint32_t jsx = (uint32_t)j * (uint32_t)(ctx->sx);
            VN_SYSI_CALL(fread(&x, sizeof(x), (size_t)1u, ctx->out) != (size_t)1u);
            const uint32_t c = normalise(x, ctx->min_val, wid);
            for (vn_integer ii = MkInt(0); ii < ctx->sy; ++ii)
              for (vn_integer jj = MkInt(0); jj < ctx->sx; ++jj)
                ps(bmp, jsx + (uint32_t)jj, isy + (uint32_t)ii, c);
          }
        }
        (void)sprintf(bmpn + idot, "-%010u.bmp", (uint32_t)k);
        if (vn_bmp_fwrite(bmp, bmpn))
          return MkInt(6);
      }
    }
  }

  if (vn_bmp_destroy(&bmp))
    return MkInt(7);

  VN_SYSI_CALL(fclose(ctx->out));
  free(memset(ctx, 0, sizeof(vn_mtxvis_ctx)));

  return MkInt(0);
}
#endif /* VN_TEST */

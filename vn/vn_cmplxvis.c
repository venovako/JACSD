#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
static void op_A(const vn_integer mA, const vn_integer nA, const vn_complex *const A, const vn_integer ldA, vn_complex *const C, const vn_integer ldC)
{
  for (vn_integer j = MkInt(0); j < nA; ++j)
    for (vn_integer i = MkInt(0); i < mA; ++i)
      C[MkIx2(i, j, ldC)] = A[MkIx2(i, j, ldA)];
}

static void op_Ah(const vn_integer mA, const vn_integer nA, const vn_complex *const A, const vn_integer ldA, vn_complex *const C, const vn_integer ldC)
{
  for (vn_integer j = MkInt(0); j < nA; ++j)
    for (vn_integer i = MkInt(0); i < mA; ++i)
      C[MkIx2(j, i, ldC)] = conj(A[MkIx2(i, j, ldA)]);
}

#ifndef VN_NO_BLAS

static void op_AAh(const vn_integer mA, const vn_integer nA, const vn_complex *const A, const vn_integer ldA, vn_complex *const C, const vn_integer ldC)
{
  static const vn_real one = MkReal(+1.0);
  static const vn_real zero = MkReal(+0.0);

  VN_BLAS_C(herk)("L", "N", (vn_integer*)&mA, (vn_integer*)&nA, (vn_real*)&one, (vn_complex*)A, (vn_integer*)&ldA, (vn_real*)&zero, C, (vn_integer*)&ldC);

  const vn_integer n = mA;
  for (vn_integer j = MkInt(0); j < n; ++j) {
    C[MkIx2(j, j, ldC)] = MkCmplx(creal(C[MkIx2(j, j, ldC)]), MkReal(+0.0));
    for (vn_integer i = j + MkInt(1); i < n; ++i)
      C[MkIx2(j, i, ldC)] = conj(C[MkIx2(i, j, ldC)]);
  }
}

static void op_AhA(const vn_integer mA, const vn_integer nA, const vn_complex *const A, const vn_integer ldA, vn_complex *const C, const vn_integer ldC)
{
  static const vn_real one = MkReal(+1.0);
  static const vn_real zero = MkReal(+0.0);

  VN_BLAS_C(herk)("L", "C", (vn_integer*)&nA, (vn_integer*)&mA, (vn_real*)&one, (vn_complex*)A, (vn_integer*)&ldA, (vn_real*)&zero, C, (vn_integer*)&ldC);

  const vn_integer n = nA;
  for (vn_integer j = MkInt(0); j < n; ++j) {
    C[MkIx2(j, j, ldC)] = MkCmplx(creal(C[MkIx2(j, j, ldC)]), MkReal(+0.0));
    for (vn_integer i = j + MkInt(1); i < n; ++i)
      C[MkIx2(j, i, ldC)] = conj(C[MkIx2(i, j, ldC)]);
  }
}

#endif /* !VN_NO_BLAS */

static vn_complex fn_0(const vn_complex x)
{
  return MkCmplx(fabs(x), carg(x));
}

static vn_complex fn_1(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log2(X)), carg(x));
}

static vn_complex fn_2(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log(X)), carg(x));
}

static vn_complex fn_3(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log10(X)), carg(x));
}

static vn_complex fn_4(const vn_complex x)
{
  return MkCmplx(fabs(x), carg(x) + M_PI);
}

static vn_complex fn_5(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log2(X)), carg(x) + M_PI);
}

static vn_complex fn_6(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log(X)), carg(x) + M_PI);
}

static vn_complex fn_7(const vn_complex x)
{
  const vn_real X = fabs(x);
  return MkCmplx(((X < VN_REAL_MIN) ? -VN_REAL_INF : log10(X)), carg(x) + M_PI);
}

static const vn_cmplxvis_fn_ptr fn_[8] = {
  fn_0, fn_1, fn_2, fn_3, fn_4, fn_5, fn_6, fn_7
};

static void apply_op(vn_cmplxvis_ctx *const ctx, const vn_complex *const A, const vn_integer ldA)
{
  (ctx->op)(ctx->mA, ctx->nA, A, ldA, ctx->C, ctx->ldC);
}

static void apply_fn(vn_cmplxvis_ctx *const ctx)
{
  for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
    for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
      vn_complex *const x = ctx->C + MkIx2(i, j, ctx->ldC);
      *x = (ctx->fn)(*x);
      const vn_real r = creal(*x);
      if (isfinite(r)) {
        if (r < creal(ctx->small))
          ((vn_real*)&(ctx->small))[0] = r;
        if (r > creal(ctx->large))
          ((vn_real*)&(ctx->large))[0] = r;
      }
      const vn_real a = cimag(*x);
      if (isfinite(a)) {
        if (a < cimag(ctx->small))
          ((vn_real*)&(ctx->small))[1] = a;
        if (a > cimag(ctx->large))
          ((vn_real*)&(ctx->large))[1] = a;
      }
    }
  }
}

static void dump_bin(vn_cmplxvis_ctx *const ctx)
{
  for (vn_integer i = MkInt(0); i < ctx->mC; ++i)
    for (vn_integer j = MkInt(0); j < ctx->nC; ++j)
      VN_SYSI_CALL(fwrite(&((ctx->C)[MkIx2(i, j, ctx->ldC)]), sizeof(vn_complex), (size_t)1u, ctx->out) != (size_t)1u);
}

vn_integer vn_cmplxvis_start(vn_cmplxvis_ctx **const ctx, const char *const fname, const vn_integer act, const vn_integer mA, const vn_integer nA, const vn_integer sx, const vn_integer sy, const vn_integer fname_len)
{
  if (!ctx)
    return MkInt(-1);
  if (!(fname && *fname))
    return MkInt(-2);
  if (act < MkInt(0))
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

  VN_SYSP_CALL(*ctx = (vn_cmplxvis_ctx*)calloc((size_t)1u, sizeof(vn_cmplxvis_ctx)));

  (*ctx)->act = act;
  (*ctx)->mA = mA;
  (*ctx)->nA = nA;

  (*ctx)->sx = sx;
  (*ctx)->sy = sy;

  (*ctx)->small = MkCmplx(+VN_REAL_INF, +VN_REAL_INF);
  (*ctx)->large = MkCmplx(-VN_REAL_INF, -VN_REAL_INF);

  switch ((*ctx)->act & MkInt(3)) {
  case VN_CMPLXVIS_OP_A:
    (*ctx)->mC = (*ctx)->mA;
    (*ctx)->nC = (*ctx)->nA;
    (*ctx)->op = op_A;
    break;
  case VN_CMPLXVIS_OP_Ah:
    (*ctx)->mC = (*ctx)->nA;
    (*ctx)->nC = (*ctx)->mA;
    (*ctx)->op = op_Ah;
    break;
#ifndef VN_NO_BLAS
  case VN_CMPLXVIS_OP_AAh:
    (*ctx)->mC = (*ctx)->mA;
    (*ctx)->nC = (*ctx)->mA;
    (*ctx)->op = op_AAh;
    break;
  case VN_CMPLXVIS_OP_AhA:
    (*ctx)->mC = (*ctx)->nA;
    (*ctx)->nC = (*ctx)->nA;
    (*ctx)->op = op_AhA;
    break;
#endif /* !VN_NO_BLAS */
  default:
    VN_DEAD_CODE;
  }

  (*ctx)->fn = fn_[((*ctx)->act >> MkInt(2)) & MkInt(7)];

  /* allocate C, set ldC */
  VN_SYSP_CALL((*ctx)->C = VN_ALLOC2(vn_complex, (*ctx)->mC, (*ctx)->nC, &((*ctx)->ldC), MkInt(-1)));

  const vn_integer fnl = ((fname_len > MkInt(7)) ? MkInt(7) : fname_len);
  vn_integer i = MkInt(0);
  for (; i < fnl; ++i)
    ((*ctx)->fname)[i] = fname[i];
  ((*ctx)->fname)[i++] = '.';
  ((*ctx)->fname)[i++] = 'd';
  ((*ctx)->fname)[i++] = 'a';
  ((*ctx)->fname)[i++] = 't';

  VN_SYSP_CALL((*ctx)->out = fopen((*ctx)->fname, "w+b"));

  const size_t len = 184u * sizeof(char);
  char *const buf = (char*)alloca(len);
  /* set first 184 B of ctx->out to newlines */
  VN_SYSI_CALL(fwrite(memset(buf, (int)'\n', len), sizeof(char), len, (*ctx)->out) != len);

  return MkInt(0);
}

vn_integer vn_cmplxvis_frame(vn_cmplxvis_ctx *const ctx, const vn_complex *const A, const vn_integer ldA)
{
  if (!ctx)
    return MkInt(-1);
  if (!A)
    return MkInt(-2);
  if (ldA <= MkInt(0))
    return MkInt(-3);

  apply_op(ctx, A, ldA);
  apply_fn(ctx);

  dump_bin(ctx);
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

vn_integer vn_cmplxvis_stop(vn_cmplxvis_ctx *const ctx)
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

  VN_SYSI_CALL((ret = fprintf(ctx->out, "minR: " VN_REAL_FMT "\n", creal(ctx->small))) != (VN_REAL_WID + 7));
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "MAXR: " VN_REAL_FMT "\n", creal(ctx->large))) != (VN_REAL_WID + 7));
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "minA: " VN_REAL_FMT "\n", cimag(ctx->small))) != (VN_REAL_WID + 7));
  tot += ret;

  VN_SYSI_CALL((ret = fprintf(ctx->out, "MAXA: " VN_REAL_FMT "\n", cimag(ctx->large))) != (VN_REAL_WID + 7));
  tot += ret;

  if (tot > 184)
    return MkInt(1);
  if (tot < 184)
    VN_SYSI_CALL(fseek(ctx->out, 184L, SEEK_SET));
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

  vn_bmp_t bmpR = (vn_bmp_t)NULL;
  if (vn_bmp_create(&bmpR, (uint32_t)(ctx->nC * ctx->sx), (int32_t)(ctx->mC * -(ctx->sy)), (uint16_t)8u))
    return MkInt(3);
  if (vn_bmp_read_cmap(bmpR, cmap))
    return MkInt(4);
  const vn_bmp_pixel_setter_t psR = vn_bmp_get_pixel_setter(bmpR);
  if (!psR)
    return MkInt(5);

  vn_bmp_t bmpA = (vn_bmp_t)NULL;
  if (vn_bmp_create(&bmpA, (uint32_t)(ctx->nC * ctx->sx), (int32_t)(ctx->mC * -(ctx->sy)), (uint16_t)8u))
    return MkInt(6);
  if (vn_bmp_read_cmap(bmpA, cmap))
    return MkInt(7);
  const vn_bmp_pixel_setter_t psA = vn_bmp_get_pixel_setter(bmpA);
  if (!psA)
    return MkInt(8);

  const vn_real widR = creal(ctx->large) - creal(ctx->small);
  const vn_real widA = cimag(ctx->large) - cimag(ctx->small);
  vn_complex z;

  for (vn_integer k = MkInt(0); k < ctx->cnt; ++k) {
    for (vn_integer i = MkInt(0); i < ctx->mC; ++i) {
      const uint32_t isy = (uint32_t)i * (uint32_t)(ctx->sy);
      for (vn_integer j = MkInt(0); j < ctx->nC; ++j) {
        const uint32_t jsx = (uint32_t)j * (uint32_t)(ctx->sx);
        VN_SYSI_CALL(fread(&z, sizeof(z), (size_t)1u, ctx->out) != (size_t)1u);
        /* compute the colour, or use the mid-value for a constant image */
        const uint32_t cR = ((widR == MkReal(0.0)) ? 127u : normalise(creal(z), creal(ctx->small), widR));
        const uint32_t cA = ((widA == MkReal(0.0)) ? 127u : normalise(cimag(z), cimag(ctx->small), widA));
        for (vn_integer ii = MkInt(0); ii < ctx->sy; ++ii) {
          for (vn_integer jj = MkInt(0); jj < ctx->sx; ++jj) {
            psR(bmpR, jsx + (uint32_t)jj, isy + (uint32_t)ii, cR);
            psA(bmpA, jsx + (uint32_t)jj, isy + (uint32_t)ii, cA);
          }
        }
      }
    }

    (void)sprintf(bmpn + idot, "R%010u.bmp", (uint32_t)k);
    if (vn_bmp_fwrite(bmpR, bmpn))
      return MkInt(9);
    (void)sprintf(bmpn + idot, "A%010u.bmp", (uint32_t)k);
    if (vn_bmp_fwrite(bmpA, bmpn))
      return MkInt(10);
  }

  if (vn_bmp_destroy(&bmpA))
    return MkInt(11);
  if (vn_bmp_destroy(&bmpR))
    return MkInt(12);

  VN_SYSI_CALL(fclose(ctx->out));
  free(memset(ctx, 0, sizeof(vn_cmplxvis_ctx)));

  return MkInt(0);
}
#endif /* ?VN_TEST */

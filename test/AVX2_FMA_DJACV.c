/* -march=native must imply at least -march=haswell (AVX2 & FMA instruction subsets) */
/* macOS: clang -std=c11   -Ofast [-DNDEBUG]               -march=native -integrated-as AVX2_FMA_DJACV.c -o AVX2_FMA_DJACV.exe -L.. -ljstrat -L$HOME/OpenBLAS-seq/lib -lopenblas */
/* macOS: clang -std=c11   -Ofast [-DNDEBUG]               -march=native -integrated-as AVX2_FMA_DJACV.c -o AVX2_FMA_DJACV.exe -L.. -ljstrat -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl */
/* Linux: gcc   -std=gnu11 -Ofast [-DNDEBUG]               -march=native                AVX2_FMA_DJACV.c -o AVX2_FMA_DJACV.exe -L.. -ljstrat -L$HOME/OpenBLAS-seq/lib -lopenblas */
/* Linux: clang -std=c11   -Ofast [-DNDEBUG] -D_GNU_SOURCE -march=native -integrated-as AVX2_FMA_DJACV.c -o AVX2_FMA_DJACV.exe -L.. -ljstrat -L$HOME/OpenBLAS-seq/lib -lopenblas */
/* -DNDEBUG => time comparison with DGESVJ for small matrix sizes, i.e., for the innermost blocking level */
#include <emmintrin.h>
#include <immintrin.h>
/* standard headers */
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
/* Jacobi strategies */
#include "../jstrat/jstrat.h"
#define VSIZE_B 32
#define DBLE_SZ 8
#define NDBLE_V (VSIZE_B / DBLE_SZ)
#define NPAIR_V NDBLE_V
#define __Int64 long
#ifdef SIGNED_INTS_ONLY
#define USGN signed
#else /* unsigneds allowed */
#define USGN unsigned
#endif /* SIGNED_INTS_ONLY */

static inline __m256d avx2_fma_ddots(const USGN int m, const double *const restrict Gp, const double *const restrict Gq)
{
  register const double *const Gp_i = (const double*)__builtin_assume_aligned(Gp, VSIZE_B);
  register const double *const Gq_i = (const double*)__builtin_assume_aligned(Gq, VSIZE_B);

  register __m256d Gpp = _mm256_setzero_pd();
  register __m256d Gqq = _mm256_setzero_pd();
  register __m256d Gpq = _mm256_setzero_pd();

  for (register USGN int i = 0; i < m; i += NDBLE_V) {
    register const __m256d Gpi = _mm256_load_pd(Gp_i + i);
    register const __m256d Gqi = _mm256_load_pd(Gq_i + i);

    Gpp = _mm256_fmadd_pd(Gpi, Gpi, Gpp);
    Gqq = _mm256_fmadd_pd(Gqi, Gqi, Gqq);
    Gpq = _mm256_fmadd_pd(Gpi, Gqi, Gpq);
  }

  register const __m256d GppGpq = _mm256_hadd_pd(Gpp, Gpq);
  register const __m256d GqqGpq = _mm256_hadd_pd(Gqq, Gpq);
  register const __m256d GppGpq_ = _mm256_permute4x64_pd(GppGpq, 0xD8);
  register const __m256d GqqGpq_ = _mm256_permute4x64_pd(GqqGpq, 0xD8);
  register const __m256d intm = _mm256_hadd_pd(GppGpq_, GqqGpq_);
  /*                                          |Gpq|, Gpq, Gqq, Gpp */
  register const __m256d mask = _mm256_set_pd(-0.0, 0.0, -0.0, -0.0);

  /* out[0] = Gpp; out[1] = Gqq; out[2] = Gpq; out[3] = |Gpq|; */
  return _mm256_andnot_pd(mask, intm);
}

#ifndef NDEBUG
static void print_vars(const double (*const A)[4])
{
  printf("Tanφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[0][0], A[0][1], A[0][2], A[0][3]);
  printf("Cosφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[1][0], A[1][1], A[1][2], A[1][3]);
  printf("Gpp`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[2][0], A[2][1], A[2][2], A[2][3]);
  printf("Gqq`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[3][0], A[3][1], A[3][2], A[3][3]);
}
#endif /* !NDEBUG */

EXTERN_C USGN __Int64 avx2_fma_djacv(const USGN int np, const USGN int m, const double tol, double *const *const restrict Gp_, double *const *const restrict Gq_, double *const *const restrict Vp_, double *const *const restrict Vq_)
{
  double res[NPAIR_V][NDBLE_V] __attribute__((aligned(VSIZE_B)));

  register const __m128i idx0 = _mm_set_epi32(12,  8, 4, 0);
  register const __m128i idx1 = _mm_set_epi32(13,  9, 5, 1);
  register const __m128i idx2 = _mm_set_epi32(14, 10, 6, 2);
  register const __m128i idx3 = _mm_set_epi32(15, 11, 7, 3);

  register const __m256d tolv = _mm256_broadcast_sd(&tol);
  register USGN int small_transf = 0;
  register USGN int big_transf = 0;
  
  for (register USGN int i = 0; i < np; i += NPAIR_V) {
    for (register USGN int j = 0; j < NPAIR_V; ++j) {
      register const USGN int ij = i + j;
      _mm256_store_pd(res[j], avx2_fma_ddots(m, (const double*)__builtin_assume_aligned(Gp_[ij], VSIZE_B), (const double*)__builtin_assume_aligned(Gq_[ij], VSIZE_B)));
    }

    register __m256d Gpp  = _mm256_i32gather_pd((const double*)res, idx0, DBLE_SZ);
    register __m256d Gqq  = _mm256_i32gather_pd((const double*)res, idx1, DBLE_SZ);

    register const __m256d Gpq  = _mm256_i32gather_pd((const double*)res, idx2, DBLE_SZ);
    register const __m256d Gpq_ = _mm256_i32gather_pd((const double*)res, idx3, DBLE_SZ);

    register const __m256d Gpp_ = _mm256_sqrt_pd(Gpp);
    register const __m256d Gqq_ = _mm256_sqrt_pd(Gqq);
    register const __m256d Max_ = _mm256_max_pd(Gpp_, Gqq_);
    register const __m256d Min_ = _mm256_min_pd(Gpp_, Gqq_);

    register const USGN int rot = _mm256_movemask_pd(_mm256_cmp_pd(Gpq_, _mm256_mul_pd(_mm256_mul_pd(Max_, tolv), Min_), _CMP_NLT_UQ));
    register USGN int swp;

    if (!rot)
      goto swapme;

    register const __m256d Gq_p = _mm256_sub_pd(Gqq, Gpp);
    register const __m256d Ctg2 = _mm256_div_pd(Gq_p, _mm256_mul_pd(Gpq, _mm256_set_pd(2.0, 2.0, 2.0, 2.0)));
    register const __m256d ones = _mm256_set_pd(1.0, 1.0, 1.0, 1.0);
    register const __m256d mask = _mm256_set_pd(-0.0, -0.0, -0.0, -0.0);
    register const __m256d Ctg = _mm256_add_pd(Ctg2, _mm256_or_pd(_mm256_sqrt_pd(_mm256_fmadd_pd(Ctg2, Ctg2, ones)), _mm256_and_pd(Ctg2, mask)));
    register const __m256d Tan = _mm256_div_pd(ones, Ctg);
    register const __m256d Cos = _mm256_div_pd(ones, _mm256_sqrt_pd(_mm256_fmadd_pd(Tan, Tan, ones)));

#ifndef NDEBUG
    /* Should never happen. */
    const USGN int Tan0 = _mm256_movemask_pd(_mm256_cmp_pd(Tan, _mm256_setzero_pd(), _CMP_EQ_UQ));

    if (Tan0 == 0x0F) /* all-zero */
      goto swapme;
#endif /* !NDEBUG */

    Gpp = _mm256_fmadd_pd(Tan, Gpq, Gpp);
    Gqq = _mm256_fnmadd_pd(Tan, Gpq, Gqq);

    register const USGN int Cos1 = _mm256_movemask_pd(_mm256_cmp_pd(Cos, ones, _CMP_EQ_UQ));
    _mm256_store_pd(res[0], Tan);
    /* if (Cos1 != 0x0F) // not all ones */
    _mm256_store_pd(res[1], Cos);
#ifndef NDEBUG
    _mm256_store_pd(res[2], Gpp);
    _mm256_store_pd(res[3], Gqq);
    print_vars(res);
#endif /* !NDEBUG */

  swapme:
    swp = _mm256_movemask_pd(_mm256_cmp_pd(Gpp, Gqq, _CMP_NGE_UQ));

    for (register USGN int j = 0; j < NPAIR_V; ++j) {
      register const USGN int msk = 1 << j;
      register const USGN int ij = i + j;
      register double *const Gpij = Gp_[ij];
      register double *const Gqij = Gq_[ij];
      register double *const Vpij = Vp_[ij];
      register double *const Vqij = Vq_[ij];
      if (rot & msk) {
        register const __m256d myTan = _mm256_broadcast_sd(&(res[0][j]));
        if (!(Cos1 & msk)) {
          ++big_transf;
          register const __m256d myCos = _mm256_broadcast_sd(&(res[1][j]));
          if (swp & msk) {
            for (register USGN int k = 0; k < m; k += NDBLE_V) {
              register const __m256d Gp_k = _mm256_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
              register const __m256d Gq_k = _mm256_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
              register const __m256d Vp_k = _mm256_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
              register const __m256d Vq_k = _mm256_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
              _mm256_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fmadd_pd(myTan, Gp_k, Gq_k)));
              _mm256_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fnmadd_pd(myTan, Gq_k, Gp_k)));
              _mm256_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fmadd_pd(myTan, Vp_k, Vq_k)));
              _mm256_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fnmadd_pd(myTan, Vq_k, Vp_k)));
            }
          }
          else {
            for (register USGN int k = 0; k < m; k += NDBLE_V) {
              register const __m256d Gp_k = _mm256_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
              register const __m256d Gq_k = _mm256_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
              register const __m256d Vp_k = _mm256_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
              register const __m256d Vq_k = _mm256_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
              _mm256_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fnmadd_pd(myTan, Gq_k, Gp_k)));
              _mm256_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fmadd_pd(myTan, Gp_k, Gq_k)));
              _mm256_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fnmadd_pd(myTan, Vq_k, Vp_k)));
              _mm256_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm256_mul_pd(myCos, _mm256_fmadd_pd(myTan, Vp_k, Vq_k)));
            }
          }
        }
        else if (swp & msk) {
          ++small_transf;
          for (register USGN int k = 0; k < m; k += NDBLE_V) {
            register const __m256d Gp_k = _mm256_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
            register const __m256d Gq_k = _mm256_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
            register const __m256d Vp_k = _mm256_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
            register const __m256d Vq_k = _mm256_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
            _mm256_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm256_fmadd_pd(myTan, Gp_k, Gq_k));
            _mm256_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm256_fnmadd_pd(myTan, Gq_k, Gp_k));
            _mm256_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm256_fmadd_pd(myTan, Vp_k, Vq_k));
            _mm256_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm256_fnmadd_pd(myTan, Vq_k, Vp_k));
          }
        }
        else {
          ++small_transf;
          for (register USGN int k = 0; k < m; k += NDBLE_V) {
            register const __m256d Gp_k = _mm256_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
            register const __m256d Gq_k = _mm256_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
            register const __m256d Vp_k = _mm256_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
            register const __m256d Vq_k = _mm256_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
            _mm256_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm256_fnmadd_pd(myTan, Gq_k, Gp_k));
            _mm256_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm256_fmadd_pd(myTan, Gp_k, Gq_k));
            _mm256_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm256_fnmadd_pd(myTan, Vq_k, Vp_k));
            _mm256_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm256_fmadd_pd(myTan, Vp_k, Vq_k));
          }
        }
      }
      else if (swp & msk) {
        ++small_transf;
        for (register USGN int k = 0; k < m; k += NDBLE_V) {
          register const __m256d Gp_k = _mm256_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
          register const __m256d Gq_k = _mm256_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
          register const __m256d Vp_k = _mm256_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
          register const __m256d Vq_k = _mm256_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
          _mm256_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), Gq_k);
          _mm256_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), Gp_k);
          _mm256_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), Vq_k);
          _mm256_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), Vp_k);
        }
      }
    }
  }

#ifndef NDEBUG
  printf("#transformations: big %d, small %d, total %d\n", big_transf, small_transf, (big_transf + small_transf));
#endif /* !NDEBUG */
  return ((((USGN __Int64)big_transf) << 32) | (USGN __Int64)small_transf);
}

EXTERN_C void dgesvj_(const char *const JOBA, const char *const JOBU, const char *const JOBV, const USGN int *const M, const USGN int *const N, double *const A, const USGN int *const LDA, double *const SVA, const USGN int *const MV, double *const V, const USGN int *const LDV, double *const WORK, const USGN int *const LWORK, int *const INFO);

EXTERN_C USGN __Int64 djaczd(const USGN int n, const USGN int m, double *const restrict G, const USGN int ldG, double *const restrict V, const USGN int ldV, int *const restrict info)
{
  const USGN int MV = n;
  const USGN int LWORK = (((m + n) < 6) ? 6 : (m + n));
  double SVA[n] __attribute__((aligned(VSIZE_B)));
  double WORK[LWORK + 1] __attribute__((aligned(VSIZE_B)));
  int *const INFO = (info ? info : (int*)(WORK + LWORK));

  dgesvj_("G", "N", "V", &m, &n, (G ? G : (WORK + LWORK)), &ldG, SVA, &MV, (V ? V : (WORK + LWORK)), &ldV, WORK, &LWORK, INFO);
  if (*INFO >= 0)
    *INFO = ((*INFO > 0) ? 31 : (int)(WORK[3]));
  return (((USGN __Int64)(WORK[1]) << 32) | (USGN __Int64)(WORK[2]));
}

EXTERN_C void dlaset_(const char *const UPLO, const USGN int *const M, const USGN int *const N, const double *const ALPHA, const double *const BETA, double *const A, const USGN int *const LDA);

EXTERN_C USGN __Int64 djacv0(const USGN int n, const USGN int m, double *const restrict G, const USGN int ldG, double *const restrict V, const USGN int ldV, int *const restrict info)
{
  static const double zero = 0.0;
  static const double one = 1.0;

  if ((n > m) || (n & 7) || (n > ldV)) {
    if (info)
      *info = -1;
    return 0;
  }
  if ((m > ldG) || (m & 3) || (m > n)) {
    if (info)
      *info = -2;
    return 0;
  }
  /* see DGESVJ, JOBU = 'N', i.e., as if CTOL = M */
  const double tol = m * (DBL_EPSILON / 2);
  /* const double tol = sqrt((double)m) * (DBL_EPSILON / 2); */

  /* Mantharam-Eberlein-like strategy. */
  jstrat_maneb2 me;
  const int stp = (int)jstrat_init((jstrat_common*)&me, (__Int64)2, (__Int64)n);
  if (stp <= 0) {
    if (info)
      *info = stp;
    return 0;
  }
  const USGN int n_2 = n >> 1;
  const USGN int np = n_2 * (USGN int)stp;

  double* Gp[np] __attribute__((aligned(VSIZE_B)));
  double* Gq[np] __attribute__((aligned(VSIZE_B)));
  double* Vp[np] __attribute__((aligned(VSIZE_B)));
  double* Vq[np] __attribute__((aligned(VSIZE_B)));

  __Int64 piv[n_2][2] __attribute__((aligned(VSIZE_B)));

  /* create the sweep */
  for (USGN int i = 0, r = 0; i < (USGN int)stp; ++i) {
    const int jnr = (int)jstrat_next((jstrat_common*)&me, piv[0]);
    if ((USGN int)jnr != n_2) {
      if (info)
        *info = jnr;
      return 0;
    }
    for (USGN int j = 0; j < n_2; ++j, ++r) {
      const USGN __Int64 p = (USGN __Int64)(piv[j][0]);
      const USGN __Int64 q = (USGN __Int64)(piv[j][1]);
      Gp[r] = G + p * ldG;
      Gq[r] = G + q * ldG;
      Vp[r] = V + p * ldV;
      Vq[r] = V + q * ldV;
    }
  }

  /* V = I, see DGESVJ, JOBV = 'V' */
  dlaset_("A", &n, &n, &zero, &one, V, &ldV);

  USGN int s = 0;
  USGN __Int64 R = 0, r;
  do {
    R += (r = avx2_fma_djacv(np, m, tol, Gp, Gq, Vp, Vq));
    if (++s >= 30)
      break;
  } while (r >= ((USGN __Int64)1 << 32));

  /* info = #sweeps */
  if (info)
    *info = (int)s;
  return R;
}

#ifdef NDEBUG
EXTERN_C void dlacpy_(const char *const UPLO, const USGN int *const M, const USGN int *const N, const double *const A, const USGN int *const LDA, double *const B, const USGN int *const LDB);
#define MAX_LDA 176
static const USGN int ldA = MAX_LDA;
static double A[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));
static double G[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));
static double V[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));
#else /* !NDEBUG */
static const USGN int C[7][4][2] __attribute__((aligned(VSIZE_B))) =
  {
    {{0,7},{1,6},{2,5},{3,4}},
    {{0,6},{1,7},{2,4},{3,5}},
    {{0,5},{1,4},{2,7},{3,6}},
    {{0,4},{1,5},{2,6},{3,7}},
    {{0,3},{1,2},{4,7},{5,6}},
    {{0,2},{1,3},{4,6},{5,7}},
    {{0,1},{2,3},{4,5},{6,7}}
  };

static double G[8][8] __attribute__((aligned(VSIZE_B))) =
  {
    {1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.5000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.2500000, 0.5000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.1250000, 0.2500000, 0.5000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0625000, 0.1250000, 0.2500000, 0.5000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 1.0000000, 0.0000000, 0.0000000},
    {0.0156250, 0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 1.0000000, 0.0000000},
    {0.0078125, 0.0156250, 0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 1.0000000}
  };

/* static double G[8][8] __attribute__((aligned(VSIZE_B))) = */
/*   { */
/*     {9.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000}, */
/*     {0.5000000, 8.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000}, */
/*     {0.2500000, 0.5000000, 7.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000}, */
/*     {0.1250000, 0.2500000, 0.5000000, 6.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000}, */
/*     {0.0625000, 0.1250000, 0.2500000, 0.5000000, 5.0000000, 0.0000000, 0.0000000, 0.0000000}, */
/*     {0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 4.0000000, 0.0000000, 0.0000000}, */
/*     {0.0156250, 0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 3.0000000, 0.0000000}, */
/*     {0.0078125, 0.0156250, 0.0312500, 0.0625000, 0.1250000, 0.2500000, 0.5000000, 2.0000000} */
/*   }; */

/* V = I_8 */
static double V[8][8] __attribute__((aligned(VSIZE_B))) =
  {
    {1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0000000, 0.0000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0000000, 0.0000000, 0.0000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0000000, 0.0000000, 0.0000000, 0.0000000, 1.0000000, 0.0000000, 0.0000000, 0.0000000},
    {0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 1.0000000, 0.0000000, 0.0000000},
    {0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 1.0000000, 0.0000000},
    {0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 1.0000000}
  };

static double* Gp_[4] __attribute__((aligned(VSIZE_B))) = { (double*)NULL, (double*)NULL, (double*)NULL, (double*)NULL };
static double* Gq_[4] __attribute__((aligned(VSIZE_B))) = { (double*)NULL, (double*)NULL, (double*)NULL, (double*)NULL };
static double* Vp_[4] __attribute__((aligned(VSIZE_B))) = { (double*)NULL, (double*)NULL, (double*)NULL, (double*)NULL };
static double* Vq_[4] __attribute__((aligned(VSIZE_B))) = { (double*)NULL, (double*)NULL, (double*)NULL, (double*)NULL };

static void print_mtx(const double (*const A)[8])
{
  for (USGN int i = 0; i < 8; ++i) {
    for (USGN int j = 0; j < 8; ++j)
      (void)fprintf(stdout, "%#26.17e", A[j][i]);
    (void)fprintf(stdout, "\n");
  }
}

static void print_step(const USGN int step)
{
  const USGN int i = step % 7;
  for (USGN int j = 0; j < 4; ++j)
    (void)fprintf(stdout, " (%d,%d)", C[i][j][0], C[i][j][1]);
  (void)fprintf(stdout, "\n");
}

static void print_matrices(const USGN int step)
{
  (void)fprintf(stdout, "\nG before step %d in cycle %d:", step, (step / 7));
  print_step(step);
  print_mtx(G);
  (void)fprintf(stdout, "\nV before step %d in cycle %d:", step, (step / 7));
  print_step(step);
  print_mtx(V);
  (void)fflush(stdout);
}

static void init_step(const USGN int step)
{
  const USGN int i = step % 7;
  for (USGN int j = 0; j < 4; ++j) {
    const USGN int p = C[i][j][0];
    const USGN int q = C[i][j][1];
    Gp_[j] = G[p];
    Gq_[j] = G[q];
    Vp_[j] = V[p];
    Vq_[j] = V[q];
  }
}
#endif /* NDEBUG */

#ifndef __APPLE__
#ifdef NDEBUG
static inline USGN __Int64 clock_gettime_nsec_np(const clockid_t clk_id)
{
  struct timespec tv;
  return (clock_gettime(clk_id, &tv) ? 0ul : (USGN __Int64)(tv.tv_sec * 1000000000ul + tv.tv_nsec));
}
#endif /* NDEBUG */
#endif /* !__APPLE__ */

int main(int argc, char* argv[])
{
#ifdef NDEBUG
  if (argc != 3) {
    (void)fprintf(stderr, "%s seed #runs\n", argv[0]);
    return EXIT_FAILURE;
  }
  const long seed = atol(argv[1]);
  if (seed >= 0)
    srand48(seed);
  for (USGN int j = 0; j < ldA; ++j)
    for (USGN int i = 0; i < ldA; ++i)
      A[j][i] = drand48();
  const USGN int nt = (USGN int)atoi(argv[2]);
  int info[2] = { 0, 0 };
  USGN __Int64 ret[2] = { 0, 0 };
  USGN __Int64 clk[2] = { 0, 0 };
  USGN __Int64 tim[2] = { 0, 0 };
  double sec[2] = { 0.0, 0.0 };
  (void)fprintf(stdout, "\"N\",\"VEC_SWP\",\"VEC_AVG_s\",\"SVJ_SWP\",\"SVJ_AVG_s\"\n");
  (void)fflush(stdout);
  for (USGN int n = 8; n <= ldA; n += 8) {
    tim[1] = tim[0] = 0;
    for (USGN int t = 0; t < nt; ++t) {
      dlacpy_("A", &n, &n, A[0], &ldA, G[0], &ldA);
      clk[0] = clock_gettime_nsec_np(CLOCK_THREAD_CPUTIME_ID);
      ret[0] = djacv0(n, n, G[0], ldA, V[0], ldA, info);
      tim[0] += (clock_gettime_nsec_np(CLOCK_THREAD_CPUTIME_ID) - clk[0]);
      dlacpy_("A", &n, &n, A[0], &ldA, G[0], &ldA);
      clk[1] = clock_gettime_nsec_np(CLOCK_THREAD_CPUTIME_ID);
      ret[1] = djaczd(n, n, G[0], ldA, V[0], ldA, info + 1);
      tim[1] += (clock_gettime_nsec_np(CLOCK_THREAD_CPUTIME_ID) - clk[1]);
    }
    sec[0] = tim[0] / (nt * 1e9);
    sec[1] = tim[1] / (nt * 1e9);
    (void)fprintf(stdout, "%3u,%9d,%#.9f,%9d,%#.9f\n", n, info[0], sec[0], info[1], sec[1]);
    (void)fflush(stdout);
  }
#else /* !NDEBUG */
  if (argc != 2) {
    (void)fprintf(stderr, "%s #steps\n", argv[0]);
    return EXIT_FAILURE;
  }

  const USGN int steps = atoi(argv[1]);
  const double tol = sqrt(8.0) * (DBL_EPSILON / 2) /* or 4 * DBL_EPSILON */;
  (void)fprintf(stdout, "#steps <= %d, tolerance = %#26.17e\n", steps, tol);

  USGN int step = 0;
  for (USGN int sweep = 0; step < steps; ++sweep) {
    USGN __Int64 ret = 0;
    for (USGN int i = 0; i < 7; ++i) {
      print_matrices(step);
      init_step(step);
      ret += avx2_fma_djacv(4, 8, tol, Gp_, Gq_, Vp_, Vq_);
      if (++step >= steps)
        break;
    }
    if (!ret)
      break;
  }
  print_matrices(step);
#endif /* NDEBUG */
  return EXIT_SUCCESS;
}

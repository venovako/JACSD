/* Intel: icc -std=gnu11 [-DNDEBUG] -D_GNU_SOURCE -O3 -xHost -no-ftz -prec-div -prec-sqrt -mkl=sequential AVX512_DJACV.c -o AVX512_DJACV.exe -L.. -ljstrat */
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
#define VSIZE_B 64
#define DBLE_SZ 8
#define NDBLE_V (VSIZE_B / DBLE_SZ)
#define NPAIR_V NDBLE_V
#define __Int64 long
#ifdef SIGNED_INTS_ONLY
#define USGN signed
#else /* unsigneds allowed */
#define USGN unsigned
#endif /* SIGNED_INTS_ONLY */

static inline __m256d avx512_ddots(const USGN int m, const double *const restrict Gp, const double *const restrict Gq)
{
  register const double *const Gp_i = (const double*)__builtin_assume_aligned(Gp, VSIZE_B);
  register const double *const Gq_i = (const double*)__builtin_assume_aligned(Gq, VSIZE_B);

  register __m512d Gpp = _mm512_setzero_pd();
  register __m512d Gqq = _mm512_setzero_pd();
  register __m512d Gpq = _mm512_setzero_pd();

  for (register USGN int i = 0; i < m; i += NDBLE_V) {
    register const __m512d Gpi = _mm512_load_pd(Gp_i + i);
    register const __m512d Gqi = _mm512_load_pd(Gq_i + i);

    Gpp = _mm512_fmadd_pd(Gpi, Gpi, Gpp);
    Gqq = _mm512_fmadd_pd(Gqi, Gqi, Gqq);
    Gpq = _mm512_fmadd_pd(Gpi, Gqi, Gpq);
  }

  register const double pq = _mm512_reduce_add_pd(Gpq);
  /* out[0] = Gpp; out[1] = Gqq; out[2] = Gpq; out[3] = |Gpq|; */
  return _mm256_set_pd(fabs(pq), pq, _mm512_reduce_add_pd(Gqq), _mm512_reduce_add_pd(Gpp));
}

#ifndef NDEBUG
static void print_vars(const double (*const A)[4])
{
  printf("LO Tanφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[0][0], A[0][1], A[0][2], A[0][3]);
  printf("HI Tanφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[1][0], A[1][1], A[1][2], A[1][3]);
  printf("LO Cosφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[2][0], A[2][1], A[2][2], A[2][3]);
  printf("HI Cosφ: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[3][0], A[3][1], A[3][2], A[3][3]);
  printf("LO Gpp`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[4][0], A[4][1], A[4][2], A[4][3]);
  printf("HI Gpp`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[5][0], A[5][1], A[5][2], A[5][3]);
  printf("LO Gqq`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[6][0], A[6][1], A[6][2], A[6][3]);
  printf("HI Gqq`: %#26.17e %#26.17e %#26.17e %#26.17e\n", A[7][0], A[7][1], A[7][2], A[7][3]);
}
#endif /* !NDEBUG */

EXTERN_C USGN __Int64 avx512_djacv(const USGN int np, const USGN int m, const double tol, double *const *const restrict Gp_, double *const *const restrict Gq_, double *const *const restrict Vp_, double *const *const restrict Vq_)
{
  double res[NPAIR_V][4] __attribute__((aligned(VSIZE_B)));

  register const __m256i idx0 = _mm256_set_epi32(28, 24, 20, 16, 12,  8, 4, 0);
  register const __m256i idx1 = _mm256_set_epi32(29, 25, 21, 17, 13,  9, 5, 1);
  register const __m256i idx2 = _mm256_set_epi32(30, 26, 22, 18, 14, 10, 6, 2);
  register const __m256i idx3 = _mm256_set_epi32(31, 27, 23, 19, 15, 11, 7, 3);

  register const __m512d tolv = _mm512_broadcastsd_pd(_mm_set_pd1(tol));
  register USGN int small_transf = 0;
  register USGN int big_transf = 0;
  
  for (register USGN int i = 0; i < np; i += NPAIR_V) {
    for (register USGN int j = 0; j < NPAIR_V; ++j) {
      register const USGN int ij = i + j;
      _mm256_store_pd(res[j], avx512_ddots(m, (const double*)__builtin_assume_aligned(Gp_[ij], VSIZE_B), (const double*)__builtin_assume_aligned(Gq_[ij], VSIZE_B)));
    }

    register __m512d Gpp  = _mm512_i32gather_pd(idx0, (const double*)res, DBLE_SZ);
    register __m512d Gqq  = _mm512_i32gather_pd(idx1, (const double*)res, DBLE_SZ);

    register const __m512d Gpq  = _mm512_i32gather_pd(idx2, (const double*)res, DBLE_SZ);
    register const __m512d Gpq_ = _mm512_i32gather_pd(idx3, (const double*)res, DBLE_SZ);

    register const __m512d Gpp_ = _mm512_sqrt_pd(Gpp);
    register const __m512d Gqq_ = _mm512_sqrt_pd(Gqq);
    register const __m512d Max_ = _mm512_max_pd(Gpp_, Gqq_);
    register const __m512d Min_ = _mm512_min_pd(Gpp_, Gqq_);

    register const USGN int rot = _mm512_cmp_pd_mask(Gpq_, _mm512_mul_pd(_mm512_mul_pd(Max_, tolv), Min_), _CMP_NLT_UQ);
    register USGN int swp;

    if (!rot)
      goto swapme;

    register const __m512d Gq_p = _mm512_sub_pd(Gqq, Gpp);
    register const __m512d twos = _mm512_broadcastsd_pd(_mm_set_pd1( 2.0));
    register const __m512d Ctg2 = _mm512_div_pd(Gq_p, _mm512_mul_pd(Gpq, twos));
    register const __m512d ones = _mm512_broadcastsd_pd(_mm_set_pd1( 1.0));
    register const __m512d mask = _mm512_broadcastsd_pd(_mm_set_pd1(-0.0));
    register const __m512d Ctg = _mm512_add_pd(Ctg2, _mm512_or_pd(_mm512_sqrt_pd(_mm512_fmadd_pd(Ctg2, Ctg2, ones)), _mm512_and_pd(Ctg2, mask)));
    register const __m512d Tan = _mm512_div_pd(ones, Ctg);
    register const __m512d Cos = _mm512_div_pd(ones, _mm512_sqrt_pd(_mm512_fmadd_pd(Tan, Tan, ones)));

#ifndef NDEBUG
    /* Should never happen. */
    const USGN int Tan0 = _mm512_cmp_pd_mask(Tan, _mm512_setzero_pd(), _CMP_EQ_UQ);

    if (Tan0 == 0x0F) /* all-zero */
      goto swapme;
#endif /* !NDEBUG */

    Gpp = _mm512_fmadd_pd(Tan, Gpq, Gpp);
    Gqq = _mm512_fnmadd_pd(Tan, Gpq, Gqq);

    register const USGN int Cos1 = _mm512_cmp_pd_mask(Cos, ones, _CMP_EQ_UQ);
    _mm512_store_pd(res[0], Tan);
    /* if (Cos1 != 0x0F) // not all ones */
    _mm512_store_pd(res[2], Cos);
#ifndef NDEBUG
    _mm512_store_pd(res[4], Gpp);
    _mm512_store_pd(res[6], Gqq);
    print_vars(res);
#endif /* !NDEBUG */

  swapme:
    swp = _mm512_cmp_pd_mask(Gpp, Gqq, _CMP_NGE_UQ);

    for (register USGN int j = 0; j < NPAIR_V; ++j) {
      register const USGN int msk = 1 << j;
      register const USGN int ij = i + j;
      register double *const Gpij = Gp_[ij];
      register double *const Gqij = Gq_[ij];
      register double *const Vpij = Vp_[ij];
      register double *const Vqij = Vq_[ij];
      if (rot & msk) {
        register const __m512d myTan = _mm512_broadcastsd_pd(_mm_set_pd1(res[0][j]));
        if (!(Cos1 & msk)) {
          ++big_transf;
          register const __m512d myCos = _mm512_broadcastsd_pd(_mm_set_pd1(res[2][j]));
          if (swp & msk) {
            for (register USGN int k = 0; k < m; k += NDBLE_V) {
              register const __m512d Gp_k = _mm512_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
              register const __m512d Gq_k = _mm512_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
              register const __m512d Vp_k = _mm512_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
              register const __m512d Vq_k = _mm512_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
              _mm512_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fmadd_pd(myTan, Gp_k, Gq_k)));
              _mm512_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fnmadd_pd(myTan, Gq_k, Gp_k)));
              _mm512_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fmadd_pd(myTan, Vp_k, Vq_k)));
              _mm512_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fnmadd_pd(myTan, Vq_k, Vp_k)));
            }
          }
          else {
            for (register USGN int k = 0; k < m; k += NDBLE_V) {
              register const __m512d Gp_k = _mm512_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
              register const __m512d Gq_k = _mm512_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
              register const __m512d Vp_k = _mm512_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
              register const __m512d Vq_k = _mm512_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
              _mm512_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fnmadd_pd(myTan, Gq_k, Gp_k)));
              _mm512_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fmadd_pd(myTan, Gp_k, Gq_k)));
              _mm512_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fnmadd_pd(myTan, Vq_k, Vp_k)));
              _mm512_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm512_mul_pd(myCos, _mm512_fmadd_pd(myTan, Vp_k, Vq_k)));
            }
          }
        }
        else if (swp & msk) {
          ++small_transf;
          for (register USGN int k = 0; k < m; k += NDBLE_V) {
            register const __m512d Gp_k = _mm512_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
            register const __m512d Gq_k = _mm512_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
            register const __m512d Vp_k = _mm512_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
            register const __m512d Vq_k = _mm512_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
            _mm512_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm512_fmadd_pd(myTan, Gp_k, Gq_k));
            _mm512_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm512_fnmadd_pd(myTan, Gq_k, Gp_k));
            _mm512_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm512_fmadd_pd(myTan, Vp_k, Vq_k));
            _mm512_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm512_fnmadd_pd(myTan, Vq_k, Vp_k));
          }
        }
        else {
          ++small_transf;
          for (register USGN int k = 0; k < m; k += NDBLE_V) {
            register const __m512d Gp_k = _mm512_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
            register const __m512d Gq_k = _mm512_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
            register const __m512d Vp_k = _mm512_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
            register const __m512d Vq_k = _mm512_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
            _mm512_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), _mm512_fnmadd_pd(myTan, Gq_k, Gp_k));
            _mm512_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), _mm512_fmadd_pd(myTan, Gp_k, Gq_k));
            _mm512_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), _mm512_fnmadd_pd(myTan, Vq_k, Vp_k));
            _mm512_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), _mm512_fmadd_pd(myTan, Vp_k, Vq_k));
          }
        }
      }
      else if (swp & msk) {
        ++small_transf;
        for (register USGN int k = 0; k < m; k += NDBLE_V) {
          register const __m512d Gp_k = _mm512_load_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B));
          register const __m512d Gq_k = _mm512_load_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B));
          register const __m512d Vp_k = _mm512_load_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B));
          register const __m512d Vq_k = _mm512_load_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B));
          _mm512_store_pd(__builtin_assume_aligned(Gpij + k, VSIZE_B), Gq_k);
          _mm512_store_pd(__builtin_assume_aligned(Gqij + k, VSIZE_B), Gp_k);
          _mm512_store_pd(__builtin_assume_aligned(Vpij + k, VSIZE_B), Vq_k);
          _mm512_store_pd(__builtin_assume_aligned(Vqij + k, VSIZE_B), Vp_k);
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

  if ((n > m) || (n & 15) || (n > ldV)) {
    if (info)
      *info = -1;
    return 0;
  }
  if ((m > ldG) || (m & 7) || (m > n)) {
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
    R += (r = avx512_djacv(np, m, tol, Gp, Gq, Vp, Vq));
    if (++s >= 30)
      break;
  } while (r >= ((USGN __Int64)1 << 32));

  /* info = #sweeps */
  if (info)
    *info = (int)s;
  return R;
}

EXTERN_C void dlacpy_(const char *const UPLO, const USGN int *const M, const USGN int *const N, const double *const A, const USGN int *const LDA, double *const B, const USGN int *const LDB);

#define MAX_LDA 256
static const USGN int ldA = MAX_LDA;

static double A[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));
static double G[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));
static double V[MAX_LDA][MAX_LDA] __attribute__((aligned(VSIZE_B)));

static inline USGN __Int64 clock_gettime_nsec_np(const clockid_t clk_id)
{
  struct timespec tv;
  return (clock_gettime(clk_id, &tv) ? 0ul : (USGN __Int64)(tv.tv_sec * 1000000000ul + tv.tv_nsec));
}

int main(int argc, char* argv[])
{
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
  for (USGN int n = 16; n <= ldA; n += 16) {
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
  return EXIT_SUCCESS;
}

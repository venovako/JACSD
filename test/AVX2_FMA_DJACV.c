/* clang -std=c11 -Ofast -march=native -integrated-as AVX2_FMA_DJACV.c */
#include <emmintrin.h>
#include <immintrin.h>

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VSIZE_B 32
#define DBLE_SZ 8
#define NDBLE_V (VSIZE_B / DBLE_SZ)
#define NPAIR_V NDBLE_V

static inline __m256 avx2_fma_ddots(const int m, const double *const restrict Gp, const double *const restrict Gq)
{
  register const double *const Gp_i = (const double*)__builtin_assume_aligned(Gp, VSIZE_B);
  register const double *const Gq_i = (const double*)__builtin_assume_aligned(Gq, VSIZE_B);

  register __m256d Gpp = _mm256_setzero_pd();
  register __m256d Gqq = _mm256_setzero_pd();
  register __m256d Gpq = _mm256_setzero_pd();

  for (register int i = 0; i < m; i += NDBLE_V) {
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

extern long long avx2_fma_djacv(const int np, const int m, const double tol, double *const *const restrict Gp_, double *const *const restrict Gq_, double *const *const restrict Vp_, double *const *const restrict Vq_)
{
  double res[NPAIR_V][NDBLE_V] __attribute__((aligned(VSIZE_B)));

  register const __m128i idx0 = _mm_set_epi32(12,  8, 4, 0);
  register const __m128i idx1 = _mm_set_epi32(13,  9, 5, 1);
  register const __m128i idx2 = _mm_set_epi32(14, 10, 6, 2);
  register const __m128i idx3 = _mm_set_epi32(15, 11, 7, 3);

  register const __m256d tolv = _mm256_broadcast_sd(&tol);
  register int small_transf = 0;
  register int big_transf = 0;
  
  for (register int i = 0; i < np; i += NPAIR_V) {
    for (register int j = 0; j < NPAIR_V; ++j) {
      register const int ij = i + j;
      _mm256_store_pd(res[j], avx2_fma_ddots(m, (const double*)__builtin_assume_aligned(Gp_[ij], VSIZE_B), (const double*)__builtin_assume_aligned(Gq_[ij], VSIZE_B)));
    }

    register __m256d Gpp  = _mm256_i32gather_pd(res, idx0, DBLE_SZ);
    register __m256d Gqq  = _mm256_i32gather_pd(res, idx1, DBLE_SZ);

    register const __m256d Gpq  = _mm256_i32gather_pd(res, idx2, DBLE_SZ);
    register const __m256d Gpq_ = _mm256_i32gather_pd(res, idx3, DBLE_SZ);

    register const __m256d Gpp_ = _mm256_sqrt_pd(Gpp);
    register const __m256d Gqq_ = _mm256_sqrt_pd(Gqq);
    register const __m256d Max_ = _mm256_max_pd(Gpp_, Gqq_);
    register const __m256d Min_ = _mm256_min_pd(Gpp_, Gqq_);

    register const int rot = _mm256_movemask_pd(_mm256_cmp_pd(Gpq_, _mm256_mul_pd(_mm256_mul_pd(Max_, tolv), Min_), _CMP_NLT_UQ));
    register int swp;

    if (!rot)
      goto swapme;

    register const __m256d Gq_p = _mm256_sub_pd(Gqq, Gpp);
    register const __m256d Ctg2 = _mm256_div_pd(Gq_p, _mm256_mul_pd(Gpq, _mm256_set_pd(2.0, 2.0, 2.0, 2.0)));
    register const __m256d ones = _mm256_set_pd(1.0, 1.0, 1.0, 1.0);
    register const __m256d mask = _mm256_set_pd(-0.0, -0.0, -0.0, -0.0);
    register const __m256d Ctg = _mm256_add_pd(Ctg2, _mm256_or_pd(_mm256_sqrt_pd(_mm256_fmadd_pd(Ctg2, Ctg2, ones)), _mm256_and_pd(Ctg2, mask)));
    register const __m256d Tan = _mm256_div_pd(ones, Ctg);
    register const __m256d Cos = _mm256_div_pd(ones, _mm256_sqrt_pd(_mm256_fmadd_pd(Tan, Tan, ones)));

    /* Should never happen.
    const int Tan0 = _mm256_movemask_pd(_mm256_cmp_pd(Tan, _mm256_setzero_pd(), _CMP_EQ_UQ));

    if (Tan0 == 0x0F) // all-zero
      goto swapme;
    */
    Gpp = _mm256_fmadd_pd(Tan, Gpq, Gpp);
    Gqq = _mm256_fnmadd_pd(Tan, Gpq, Gqq);

    register const int Cos1 = _mm256_movemask_pd(_mm256_cmp_pd(Cos, ones, _CMP_EQ_UQ));
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

    for (register int j = 0; j < NPAIR_V; ++j) {
      register const int msk = 1 << j;
      register const int ij = i + j;
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
            for (register int k = 0; k < m; k += NDBLE_V) {
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
            for (register int k = 0; k < m; k += NDBLE_V) {
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
          for (register int k = 0; k < m; k += NDBLE_V) {
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
          for (register int k = 0; k < m; k += NDBLE_V) {
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
        for (register int k = 0; k < m; k += NDBLE_V) {
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
  return ((((long long)big_transf) << 32) | (long long)small_transf);
}

#ifndef NDEBUG
static const int C[7][4][2] __attribute__((aligned(VSIZE_B))) =
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
  for (int i = 0; i < 8; ++i) {
    for (int j = 0; j < 8; ++j)
      printf("%#26.17e", A[j][i]);
    printf("\n");
  }
}

static void print_step(const int step)
{
  const int i = step % 7;
  for (int j = 0; j < 4; ++j)
    printf(" (%d,%d)", C[i][j][0], C[i][j][1]);
  printf("\n");
}

static void print_matrices(const int step)
{
  printf("\nG before step %d:", step);
  print_step(step);
  print_mtx(G);
  printf("\nV before step %d:", step);
  print_step(step);
  print_mtx(V);
  fflush(stdout);
}

static void init_step(const int step)
{
  const int i = step % 7;
  for (int j = 0; j < 4; ++j) {
    const int p = C[i][j][0];
    const int q = C[i][j][1];
    Gp_[j] = G[p];
    Gq_[j] = G[q];
    Vp_[j] = V[p];
    Vq_[j] = V[q];
  }
}

int main(int argc, char *argv[])
{
  if (argc != 2) {
    fprintf(stderr, "%s #steps\n", argv[0]);
    return EXIT_FAILURE;
  }

  const int steps = atoi(argv[1]);
  const double tol = sqrt(8.0) * (DBL_EPSILON / 2)/*4.0 * DBL_EPSILON*/;
  printf("#steps: %d, tolerance: %#26.17e\n", steps, tol);

  for (int i = 0; i < steps; ++i) {
    print_matrices(i);
    init_step(i);
    avx2_fma_djacv(4, 8, tol, Gp_, Gq_, Vp_, Vq_);
  }
  print_matrices(steps);

  return EXIT_SUCCESS;
}
#endif /* !NDEBUG */

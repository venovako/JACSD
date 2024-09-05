#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif /* !_GNU_SOURCE */

#include <emmintrin.h>
#include <immintrin.h>
// standard headers
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "AVX2_FMA_DJACV.h"

#define VSIZE_B 32u
#define DBLE_SZ 8u
#define NDBLE_V (VSIZE_B / DBLE_SZ)
#define NPAIR_V NDBLE_V

static inline __m256d avx2_fma_ddots(const unsigned m, const double *const restrict Gp, const double *const restrict Gq)
{
  register const double *const Gp_i = (const double*)__builtin_assume_aligned(Gp, VSIZE_B);
  register const double *const Gq_i = (const double*)__builtin_assume_aligned(Gq, VSIZE_B);

  register __m256d Gpp = _mm256_setzero_pd();
  register __m256d Gqq = _mm256_setzero_pd();
  register __m256d Gpq = _mm256_setzero_pd();

  for (register unsigned int i = 0u; i < m; i += NDBLE_V) {
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
  //                                          |Gpq|, Gpq, Gqq, Gpp
  register const __m256d mask = _mm256_set_pd(-0.0, 0.0, -0.0, -0.0);

  // out[0] = Gpp; out[1] = Gqq; out[2] = Gpq; out[3] = |Gpq|;
  return _mm256_andnot_pd(mask, intm);
}

static unsigned long avx2_fma_djacv(const unsigned np, const unsigned m, double *const restrict G, double *const restrict V, const double tol, uintptr_t *const restrict Sp_, uintptr_t *const restrict Sq_)
{
  double res[NPAIR_V][NDBLE_V] __attribute__((aligned(VSIZE_B)));

  register const __m128i idx0 = _mm_set_epi32(12,  8, 4, 0);
  register const __m128i idx1 = _mm_set_epi32(13,  9, 5, 1);
  register const __m128i idx2 = _mm_set_epi32(14, 10, 6, 2);
  register const __m128i idx3 = _mm_set_epi32(15, 11, 7, 3);

  register const __m256d tolv = _mm256_broadcast_sd(&tol);
  register unsigned small_transf = 0u;
  register unsigned big_transf = 0u;
  
  for (register unsigned i = 0u; i < np; i += NPAIR_V) {
    for (register unsigned j = 0u; j < NPAIR_V; ++j) {
      register const unsigned ij = i + j;
      _mm256_store_pd(res[j], avx2_fma_ddots(m, (const double*)__builtin_assume_aligned((G + Sp_[ij]), VSIZE_B), (const double*)__builtin_assume_aligned((G + Sq_[ij]), VSIZE_B)));
    }

    register __m256d Gpp  = _mm256_i32gather_pd((const double*)res, idx0, DBLE_SZ);
    register __m256d Gqq  = _mm256_i32gather_pd((const double*)res, idx1, DBLE_SZ);

    register const __m256d Gpq  = _mm256_i32gather_pd((const double*)res, idx2, DBLE_SZ);
    register const __m256d Gpq_ = _mm256_i32gather_pd((const double*)res, idx3, DBLE_SZ);

    register const __m256d Gpp_ = _mm256_sqrt_pd(Gpp);
    register const __m256d Gqq_ = _mm256_sqrt_pd(Gqq);
    register const __m256d Max_ = _mm256_max_pd(Gpp_, Gqq_);
    register const __m256d Min_ = _mm256_min_pd(Gpp_, Gqq_);

    register const unsigned rot = (unsigned)_mm256_movemask_pd(_mm256_cmp_pd(Gpq_, _mm256_mul_pd(_mm256_mul_pd(Max_, tolv), Min_), _CMP_NLT_UQ));
    register unsigned swp;

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
    // Should never happen.
    const unsigned Tan0 = (unsigned)_mm256_movemask_pd(_mm256_cmp_pd(Tan, _mm256_setzero_pd(), _CMP_EQ_UQ));

    if (Tan0 == 0x0Fu) // all-zero
      goto swapme;
#endif /* !NDEBUG */

    Gpp = _mm256_fmadd_pd(Tan, Gpq, Gpp);
    Gqq = _mm256_fnmadd_pd(Tan, Gpq, Gqq);

    register const unsigned Cos1 = (unsigned)_mm256_movemask_pd(_mm256_cmp_pd(Cos, ones, _CMP_EQ_UQ));
    _mm256_store_pd(res[0], Tan);
    /* if (Cos1 != 0x0Fu) // not all ones */
    _mm256_store_pd(res[1], Cos);

  swapme:
    swp = (unsigned)_mm256_movemask_pd(_mm256_cmp_pd(Gpp, Gqq, _CMP_NGE_UQ));

    for (register unsigned j = 0u; j < NPAIR_V; ++j) {
      register const unsigned msk = 1u << j;
      register const unsigned ij = i + j;
      register double *const Gpij = G + Sp_[ij];
      register double *const Gqij = G + Sq_[ij];
      register double *const Vpij = V + Sp_[ij];
      register double *const Vqij = V + Sq_[ij];
      if (rot & msk) {
        register const __m256d myTan = _mm256_broadcast_sd(&(res[0][j]));
        if (!(Cos1 & msk)) {
          ++big_transf;
          register const __m256d myCos = _mm256_broadcast_sd(&(res[1][j]));
          if (swp & msk) {
            for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
            for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
          for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
          for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
        for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
  return (((unsigned long)big_transf << 32u) | (unsigned long)small_transf);
}

unsigned long avx2_fma_djacv0_execute(const unsigned m, const unsigned n, double *const restrict G, double *const restrict V, const double tol, const int swp, uintptr_t *const restrict Iwork, int *const restrict info)
{
  assert(swp);
  assert(Iwork);
  assert(info);

  const unsigned n_2 = n >> 1u;
  const unsigned stp = ((swp < 0) ? n : (n - 1u));
  const unsigned swp_ = (unsigned)abs(swp);
  const unsigned np = n_2 * stp;
  const unsigned np_ = n_2 * n;

  uintptr_t *Sp = (uintptr_t*)__builtin_assume_aligned(Iwork, VSIZE_B);
  uintptr_t *Sq = (uintptr_t*)__builtin_assume_aligned((Sp + np_), VSIZE_B);

  unsigned s = 0u;
  unsigned long R = 0ul, r;
  do {
    R += (r = avx2_fma_djacv(np, m, G, V, tol, Sp, Sq));
    if (++s >= swp_)
      break;
  } while (r >= (1ul << 32u));

  // info = #sweeps
  *info = (int)s;
  return R;
}

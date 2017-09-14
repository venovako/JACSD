#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif /* !_GNU_SOURCE */

#include <emmintrin.h>
#include <immintrin.h>
/* standard headers */
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "AVX512_DJACV.h"

#define VSIZE_B 64u
#define DBLE_SZ 8u
#define NDBLE_V (VSIZE_B / DBLE_SZ)
#define NPAIR_V NDBLE_V

static inline __m256d avx512_ddots(const unsigned m, const double *const restrict Gp, const double *const restrict Gq)
{
  register const double *const Gp_i = (const double*)__builtin_assume_aligned(Gp, VSIZE_B);
  register const double *const Gq_i = (const double*)__builtin_assume_aligned(Gq, VSIZE_B);

  register __m512d Gpp = _mm512_setzero_pd();
  register __m512d Gqq = _mm512_setzero_pd();
  register __m512d Gpq = _mm512_setzero_pd();

  for (register unsigned i = 0u; i < m; i += NDBLE_V) {
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

static unsigned long avx512_djacv(const unsigned np, const unsigned m, double *const restrict G, double *const restrict V, const double tol, uintptr_t *const restrict Sp_, uintptr_t *const restrict Sq_)
{
  double res[NPAIR_V][4] __attribute__((aligned(VSIZE_B)));

  register const __m256i idx0 = _mm256_set_epi32(28, 24, 20, 16, 12,  8, 4, 0);
  register const __m256i idx1 = _mm256_set_epi32(29, 25, 21, 17, 13,  9, 5, 1);
  register const __m256i idx2 = _mm256_set_epi32(30, 26, 22, 18, 14, 10, 6, 2);
  register const __m256i idx3 = _mm256_set_epi32(31, 27, 23, 19, 15, 11, 7, 3);

  register const __m512d tolv = _mm512_broadcastsd_pd(_mm_set_pd1(tol));
  register unsigned small_transf = 0u;
  register unsigned big_transf = 0u;
  
  for (register unsigned i = 0u; i < np; i += NPAIR_V) {
    for (register unsigned j = 0u; j < NPAIR_V; ++j) {
      register const unsigned ij = i + j;
      _mm256_store_pd(res[j], avx512_ddots(m, (const double*)__builtin_assume_aligned((G + Sp_[ij]), VSIZE_B), (const double*)__builtin_assume_aligned((G + Sq_[ij]), VSIZE_B)));
    }

    register __m512d Gpp  = _mm512_i32gather_pd(idx0, (const double*)res, DBLE_SZ);
    register __m512d Gqq  = _mm512_i32gather_pd(idx1, (const double*)res, DBLE_SZ);

    register const __m512d Gpq  = _mm512_i32gather_pd(idx2, (const double*)res, DBLE_SZ);
    register const __m512d Gpq_ = _mm512_i32gather_pd(idx3, (const double*)res, DBLE_SZ);

    register const __m512d Gpp_ = _mm512_sqrt_pd(Gpp);
    register const __m512d Gqq_ = _mm512_sqrt_pd(Gqq);
    register const __m512d Max_ = _mm512_max_pd(Gpp_, Gqq_);
    register const __m512d Min_ = _mm512_min_pd(Gpp_, Gqq_);

    register const unsigned rot = (unsigned)_mm512_cmp_pd_mask(Gpq_, _mm512_mul_pd(_mm512_mul_pd(Max_, tolv), Min_), _CMP_NLT_UQ);
    register unsigned swp;

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
    const unsigned Tan0 = (unsigned)_mm512_cmp_pd_mask(Tan, _mm512_setzero_pd(), _CMP_EQ_UQ);

    if (Tan0 == 0x0Fu) /* all-zero */
      goto swapme;
#endif /* !NDEBUG */

    Gpp = _mm512_fmadd_pd(Tan, Gpq, Gpp);
    Gqq = _mm512_fnmadd_pd(Tan, Gpq, Gqq);

    register const unsigned Cos1 = (unsigned)_mm512_cmp_pd_mask(Cos, ones, _CMP_EQ_UQ);
    _mm512_store_pd(res[0], Tan);
    /* if (Cos1 != 0x0Fu) // not all ones */
    _mm512_store_pd(res[2], Cos);

  swapme:
    swp = (unsigned)_mm512_cmp_pd_mask(Gpp, Gqq, _CMP_NGE_UQ);

    for (register unsigned j = 0u; j < NPAIR_V; ++j) {
      register const unsigned msk = 1u << j;
      register const unsigned ij = i + j;
      register double *const Gpij = G + Sp_[ij];
      register double *const Gqij = G + Sq_[ij];
      register double *const Vpij = V + Sp_[ij];
      register double *const Vqij = V + Sq_[ij];
      if (rot & msk) {
        register const __m512d myTan = _mm512_broadcastsd_pd(_mm_set_pd1(res[0][j]));
        if (!(Cos1 & msk)) {
          ++big_transf;
          register const __m512d myCos = _mm512_broadcastsd_pd(_mm_set_pd1(res[2][j]));
          if (swp & msk) {
            for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
            for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
          for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
          for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
        for (register unsigned k = 0u; k < m; k += NDBLE_V) {
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
  return (((unsigned long)big_transf << 32u) | (unsigned long)small_transf);
}

unsigned long avx512_djacv0_execute(const unsigned m, const unsigned n, double *const restrict G, double *const restrict V, const double tol, const int swp, uintptr_t *const restrict Iwork, int *const restrict info)
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
    R += (r = avx512_djacv(np, m, G, V, tol, Sp, Sq));
    if (++s >= swp_)
      break;
  } while (r >= (1ul << 32u));

  /* info = #sweeps */
  *info = (int)s;
  return R;
}

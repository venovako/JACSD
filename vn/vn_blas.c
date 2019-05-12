#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
#ifndef VN_NO_BLAS
static volatile bool vn_blas_prepared = false;

vn_integer vn_blas_prepare()
{
  if (!vn_blas_prepared) {
#ifdef _OPENMP
    if (!omp_get_nested())
      omp_set_nested(1);
    if (!omp_get_dynamic())
      omp_set_dynamic(1);
#endif /* _OPENMP */
#ifdef USE_MKL
    if (!mkl_get_dynamic())
      mkl_set_dynamic(1);
#ifndef NDEBUG
    if (mkl_peak_mem_usage(MKL_PEAK_MEM_ENABLE) == (MKL_INT64)(-1))
      return MkInt(-1);
    if (mkl_verbose(1) == -1)
      return MkInt(-2);
#endif /* !NDEBUG */
#endif /* USE_MKL */
    vn_blas_prepared = true;
  }
#ifdef _OPENMP
  return (vn_integer)omp_get_max_active_levels();
#else /* !_OPENMP */
  return MkInt(0);
#endif /* ?_OPENMP */
}

vn_integer vn_blas_finish(vn_integer *const curr, vn_integer *const nbuf)
{
  vn_integer a = MkInt(0);
  vn_integer b = MkInt(0);
  vn_integer c = MkInt(0);
  if (vn_blas_prepared) {
#ifdef USE_MKL
#ifndef NDEBUG
    c = (vn_integer)mkl_mem_stat((int*)&b);
    a = (vn_integer)mkl_peak_mem_usage(MKL_PEAK_MEM_RESET);
#endif /* NDEBUG */
    mkl_finalize();
#else /* !USE_MKL */
#endif /* ?USE_MKL */
    vn_blas_prepared = false;
  }
  if (curr)
    *curr = c;
  if (nbuf)
    *nbuf = b;
  return a;
}

vn_integer vn_blas_set_num_threads(const vn_integer nt)
{
  if (nt < MkInt(0))
    return MkInt(-1);
#ifdef USE_MKL
  return (vn_integer)mkl_set_num_threads_local((int)nt);
#else /* !USE_MKL */
#ifdef _OPENMP
  vn_integer ret = MkInt(-2);
  if (nt) {
    ret = (vn_integer)omp_get_num_threads();
    omp_set_num_threads((int)nt);
  }
#else /* !_OPENMP */
  const vn_integer ret = MkInt(0);
#endif /* ?_OPENMP */
  return ret;
#endif /* ?USE_MKL */
}
#endif /* !VN_NO_BLAS */
#endif /* VN_TEST */

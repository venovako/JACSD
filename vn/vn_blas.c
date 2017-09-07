#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */

#ifdef _OPENMP
#include <omp.h>
#endif /* _OPENMP */

vn_integer vn_blas_prepare()
{
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
  switch ((vn_integer)mkl_peak_mem_usage(MKL_PEAK_MEM_ENABLE)) {
  case MkInt(-1):
    return MkInt(-1);
  }
  switch (mkl_verbose(1)) {
  case -1:
    return MkInt(-2);
  }
#endif /* !NDEBUG */
#endif /* USE_MKL */
#ifdef _OPENMP
  return (vn_integer)omp_get_max_active_levels();
#else /* !_OPENMP */
  return MkInt(0);
#endif /* ?_OPENMP */
}

vn_integer vn_blas_finish(vn_integer *const curr, vn_integer *const nbuf)
{
#ifdef USE_MKL
#ifndef NDEBUG
  vn_integer a = (vn_integer)mkl_peak_mem_usage(MKL_PEAK_MEM);
  switch (a) {
  case MkInt(-1):
    return MkInt(-1);
  }
  vn_integer b = (vn_integer)mkl_peak_mem_usage(MKL_PEAK_MEM_DISABLE);
  switch (b) {
  case MkInt(-1):
    return MkInt(-2);
  }
  b = MkInt(0);
  vn_integer c = mkl_mem_stat((int*)&b);
  switch (c) {
  case MkInt(-1):
    return MkInt(-3);
  }
  if (curr)
    *curr = c;
  if (nbuf)
    *nbuf = b;
  return a;
#endif /* !NDEBUG */
#endif /* USE_MKL */
  return MkInt(0);
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
#endif /* VN_TEST */

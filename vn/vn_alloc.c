#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  const vn_integer m = MkInt(3);
  const vn_integer n = MkInt(2);
  const vn_integer act = MkInt(-1);
  vn_integer ldA = MkInt(0);
  vn_real *const A = VN_ALLOC2(vn_real, m, n, &ldA, act);
  VN_IS_ALIGNED2(A, ldA);
  VN_SYSI_CALL(printf("ldA: %d\n", ldA) <= 0);
  free(A);
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
void *vn_alloc1(const vn_integer m, const size_t szT, vn_integer *const ldA, const vn_integer act)
{
  return vn_alloc2(m, MkInt(1), szT, ldA, act);
}

void *vn_alloc2(const vn_integer m, const vn_integer n, const size_t szT, vn_integer *const ldA, const vn_integer act)
{
  VN_ASSERT(m >= MkInt(0));
  VN_ASSERT(n >= MkInt(0));
  void *ret = NULL;
  vn_integer ld_ = MkInt(0);
  if ((m > MkInt(0)) && (n > MkInt(0)) && szT) {
    if ((szT <= VN_ALIGN_BYTES) && !(VN_ALIGN_BYTES % szT)) {
      const vn_integer noe = VN_ALIGN_BYTES / szT;
      const vn_integer rem = m % noe;
      if (rem)
        ld_ = m + (noe - rem);
      else
        ld_ = m;
    }
    else
      ld_ = m;
    if (act) {
      const size_t siz = ld_ * (n * szT);
      VN_SYSI_CALL(posix_memalign(&ret, VN_ALIGN_BYTES, siz));
      if (ret && (act == MkInt(-1)))
        (void)memset(ret, 0, siz);
    }
  }
  if (ldA)
    *ldA = ld_;
  return ret;
}
#endif /* VN_TEST */

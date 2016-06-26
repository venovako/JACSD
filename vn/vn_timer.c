#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  const vn_integer_8 ret = vn_timer();
  VN_SYSI_CALL(printf("%lld\n", ret) <= 0);
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
vn_integer_8 vn_timer()
{
  struct timeval t;
  VN_SYSI_CALL(gettimeofday(&t, NULL));
  return (t.tv_sec * (vn_integer_8)1000000 + t.tv_usec);
}
#endif /* VN_TEST */

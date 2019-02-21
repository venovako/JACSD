#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  VN_SYSI_CALL(printf("vn_get_sys_us = %lld\n", (long long)vn_get_sys_us()) <= 0);
  VN_SYSI_CALL(printf("vn_get_thread_ns = %lld\n", (long long)vn_get_thread_ns()) <= 0);
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
vn_integer_8 vn_get_thread_ns()
{
  struct timespec tp;
  if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tp))
    return (vn_integer_8)-1;
  return (tp.tv_sec * (vn_integer_8)1000000000 + tp.tv_nsec);
}

vn_integer_8 vn_get_sys_us()
{
  struct timeval tv;
  if (gettimeofday(&tv, NULL))
    return (vn_integer_8)-1;
  return (tv.tv_sec * (vn_integer_8)1000000 + tv.tv_usec);
}

#endif /* VN_TEST */

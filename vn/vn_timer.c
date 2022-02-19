#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  VN_SYSI_CALL(printf("vn_get_sys_us = %ld\n", vn_get_sys_us()) <= 0);
  VN_SYSI_CALL(printf("vn_get_thread_ns = %ld\n", vn_get_thread_ns()) <= 0);
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
long vn_get_thread_ns()
{
  struct timespec t;
  return (clock_gettime(CLOCK_THREAD_CPUTIME_ID, &t) ? -1L : t2ns(&t));
}

long vn_get_sys_us()
{
  struct timeval t;
  return (gettimeofday(&t, NULL) ? -1L : t2us(&t));
}

uint64_t rdtsc_beg_(unsigned *const aux)
{
  return (aux ? rdtsc_beg(aux) : UINT64_C(0));
}

uint64_t rdtsc_end_(unsigned *const aux)
{
  return (aux ? rdtsc_end(aux) : UINT64_C(0));
}

uint64_t tsc_get_freq_hz_(unsigned *const rem_den)
{
  return tsc_get_freq_hz(rem_den);
}

double tsc_lap_(const uint64_t freq_hz, const uint64_t beg, const uint64_t end, uint64_t *const sec, uint64_t *const rem)
{
  return (double)tsc_lap(freq_hz, beg, end, sec, rem);
}
#endif /* ?VN_TEST */

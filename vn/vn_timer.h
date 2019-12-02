#ifndef VN_TIMER_H
#define VN_TIMER_H

#ifndef VN_LIB_H
#error vn_timer.h not intended for direct inclusion
#endif /* !VN_LIB_H */

VN_EXTERN_C vn_integer_8 vn_get_thread_ns();
VN_EXTERN_C vn_integer_8 vn_get_sys_us();

/* see Intel(R) 64 and IA-32 Architectures Software Developer's Manual */

#include <x86intrin.h>
#ifdef TSC_FREQ_HZ
#if (TSC_FREQ_HZ == 0ull)
#include <cpuid.h>
#endif /* ?TSC_FREQ_HZ */
#endif /* TSC_FREQ_HZ */

static inline uint64_t rdtsc_beg(unsigned *const aux)
{
  _mm_mfence();
  return __rdtscp(aux);
}

static inline uint64_t rdtsc_end(unsigned *const aux)
{
  const uint64_t tsc = __rdtscp(aux);
  _mm_lfence();
  return tsc;
}

static inline uint64_t tsc_get_freq_hz()
{
#ifdef TSC_FREQ_HZ
#if (TSC_FREQ_HZ == 0ull)
  unsigned eax = 0x15u, ebx = 0u, ecx = 0u, edx = 0u;
  __cpuid(0x15u, eax, ebx, ecx, edx);
  return (eax ? (((uint64_t)ebx * ecx) / eax) : UINT64_C(0));
#else /* TSC_FREQ_HZ > 0 */
  return TSC_FREQ_HZ;
#endif /* ?TSC_FREQ_HZ */
#else /* !TSC_FREQ_HZ */
  uint64_t hz = UINT64_C(0);
#ifdef __APPLE__
  size_t hzl = sizeof(hz);
  if (sysctlbyname("machdep.tsc.frequency", &hz, &hzl, NULL, (size_t)0u))
    perror("tsc_get_freq_hz:sysctlbyname");
#else /* Linux */
  /* see https://github.com/trailofbits/tsc_freq_khz */
  FILE *const f = fopen("/sys/devices/system/cpu/cpu0/tsc_freq_hz", "r");
  if (f) {
    if (fscanf(f, "%llu", &hz) != 1)
      perror("tsc_get_freq_hz:fscanf");
    else /* successful read */
      hz *= UINT64_C(1000);
    if (fclose(f))
      perror("tsc_get_freq_hz:fclose");
  }
  else
    perror("tsc_get_freq_hz:fopen");
#endif /* ?__APPLE__ */

  return hz;
#endif /* ?TSC_FREQ_HZ */
}

static inline double tsc_lap(const uint64_t freq_hz, const uint64_t beg, const uint64_t end, int64_t *const sec, int64_t *const rem)
{
  const long long freq_hz_ = (long long)freq_hz;
  if (freq_hz <= 0ll)
    return -1;
  const long long beg_ = (long long)beg;
  if (beg_ < 0ll)
    return -2;
  const long long end_ = (long long)end;
  if (end_ < 0ll)
    return -3;
  const long long end_beg = end - beg;
  if (end_beg < 0ll)
    return -4;
  const lldiv_t qr = lldiv(end_beg, freq_hz_);
  if (sec)
    *sec = (int64_t)(qr.quot);
  if (rem)
    *rem = (int64_t)(qr.rem);
  return ((double)end_beg / (double)freq_hz_);
}

VN_EXTERN_C uint64_t rdtsc_beg_(unsigned *const aux);
VN_EXTERN_C uint64_t rdtsc_end_(unsigned *const aux);
VN_EXTERN_C uint64_t tsc_get_freq_hz_();
VN_EXTERN_C double tsc_lap_(const uint64_t freq_hz, const uint64_t beg, const uint64_t end, int64_t *const sec, int64_t *const rem);

#endif /* !VN_TIMER_H */

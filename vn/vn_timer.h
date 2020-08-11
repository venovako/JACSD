#ifndef VN_TIMER_H
#define VN_TIMER_H

#ifndef VN_LIB_H
#error vn_timer.h not intended for direct inclusion
#endif /* !VN_LIB_H */

VN_EXTERN_C vn_integer_8 vn_get_thread_ns();
VN_EXTERN_C vn_integer_8 vn_get_sys_us();

// see Intel(R) 64 and IA-32 Architectures Software Developer's Manual

static inline uint64_t VN_NO_PROF rdtsc_beg(unsigned *const aux)
{
#ifdef USE_NVIDIA
  return UINT64_C(0);
#else /* !USE_NVIDIA */
  _mm_mfence();
  return __rdtscp(aux);
#endif /* ?USE_NVIDIA */
}

static inline uint64_t VN_NO_PROF rdtsc_end(unsigned *const aux)
{
#ifdef USE_NVIDIA
  return UINT64_C(0);
#else /* !USE_NVIDIA */
  const uint64_t tsc = __rdtscp(aux);
  _mm_lfence();
  return tsc;
#endif /* ?USE_NVIDIA */
}

static inline uint64_t VN_NO_PROF tsc_get_freq_hz(unsigned *const rem_den)
{
#ifdef TSC_FREQ_HZ
#if (TSC_FREQ_HZ == 0ull)
  unsigned eax = 0u, ebx = 0u, ecx = 0u, edx = 0u;
  __cpuid(0x15u, eax, ebx, ecx, edx);
  if (eax) {
    const uint64_t num = (uint64_t)ebx * ecx;
    const uint64_t den = (uint64_t)eax;
    if (rem_den) {
      rem_den[0u] = (unsigned)(num % den);
      rem_den[1u] = eax;
    }
    return (num / den);
  }
  else {
    if (rem_den) {
      rem_den[0u] = 0u;
      rem_den[1u] = 0u;
    }
    return UINT64_C(0);
  }
#else /* TSC_FREQ_HZ > 0 */
  if (rem_den) {
    rem_den[0u] = 0u;
    rem_den[1u] = 0u;
  }
  return (TSC_FREQ_HZ);
#endif /* ?TSC_FREQ_HZ */
#else /* !TSC_FREQ_HZ */
  uint64_t hz = UINT64_C(0);
#ifdef __APPLE__
  size_t hzl = sizeof(hz);
  if (sysctlbyname("machdep.tsc.frequency", &hz, &hzl, NULL, (size_t)0u))
    perror("tsc_get_freq_hz:sysctlbyname");
#else /* Linux */
  // see https://github.com/trailofbits/tsc_freq_khz
  FILE *const f = fopen("/sys/devices/system/cpu/cpu0/tsc_freq_hz", "r");
  if (f) {
    if (fscanf(f, "%llu", &hz) != 1)
      perror("tsc_get_freq_hz:fscanf");
    else // successful read
      hz *= UINT64_C(1000);
    if (fclose(f))
      perror("tsc_get_freq_hz:fclose");
  }
  else
    perror("tsc_get_freq_hz:fopen");
#endif /* ?__APPLE__ */
  if (rem_den) {
    rem_den[0u] = 0u;
    rem_den[1u] = 0u;
  }
  return hz;
#endif /* ?TSC_FREQ_HZ */
}

static inline double VN_NO_PROF tsc_lap(const uint64_t freq_hz, const uint64_t beg, const uint64_t end, uint64_t *const sec, uint64_t *const rem)
{
  if (freq_hz) {
    if (end >= beg) {
      const uint64_t lap = end - beg;
      if (sec)
        *sec = lap / freq_hz;
      if (rem)
        *rem = lap % freq_hz;
      if (lap >= freq_hz)
        return (lap / (double)freq_hz);
      else // lap < freq_hz
        return ((double)lap / freq_hz);
    }
    else {
      if (sec)
        *sec = UINT64_C(0);
      if (rem)
        *rem = UINT64_C(0);
      return -0.0;
    }
  }
  else {
    if (sec)
      *sec = UINT64_C(0);
    if (rem)
      *rem = UINT64_C(0);
    return MqNaN(0);
  }
}

static inline long VN_NO_PROF t2ns(const struct timespec tp[static 1])
{
  return (tp->tv_sec * 1000000000L + tp->tv_nsec);
}

static inline long VN_NO_PROF t2us(const struct timeval tp[static 1])
{
  return (tp->tv_sec * 1000000L + tp->tv_usec);
}

VN_EXTERN_C uint64_t rdtsc_beg_(unsigned *const aux);
VN_EXTERN_C uint64_t rdtsc_end_(unsigned *const aux);
VN_EXTERN_C uint64_t tsc_get_freq_hz_(unsigned *const rem_den);
VN_EXTERN_C double tsc_lap_(const uint64_t freq_hz, const uint64_t beg, const uint64_t end, uint64_t *const sec, uint64_t *const rem);

#endif /* !VN_TIMER_H */

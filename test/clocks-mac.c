#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

int main()
{
  struct timespec tv;
  fprintf(stdout, "sizeof(time_t) = %zu, sizeof(clock_t) = %zu, CLOCKS_PER_SEC = %u.\n", sizeof(time_t), sizeof(clock_t), CLOCKS_PER_SEC);
  if (clock_getres(CLOCK_REALTIME, &tv))
    fprintf(stderr, "CLOCK_REALTIME not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_REALTIME resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_REALTIME resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_MONOTONIC, &tv))
    fprintf(stderr, "CLOCK_MONOTONIC not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_MONOTONIC resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_MONOTONIC resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_MONOTONIC_RAW, &tv))
    fprintf(stderr, "CLOCK_MONOTONIC_RAW not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_MONOTONIC_RAW resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_MONOTONIC_RAW resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_MONOTONIC_RAW_APPROX, &tv))
    fprintf(stderr, "CLOCK_MONOTONIC_RAW_APPROX not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_MONOTONIC_RAW_APPROX resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_MONOTONIC_RAW_APPROX resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_UPTIME_RAW, &tv))
    fprintf(stderr, "CLOCK_UPTIME_RAW not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_UPTIME_RAW resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_UPTIME_RAW resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_UPTIME_RAW_APPROX, &tv))
    fprintf(stderr, "CLOCK_UPTIME_RAW_APPROX not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_UPTIME_RAW_APPROX resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_UPTIME_RAW_APPROX resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_PROCESS_CPUTIME_ID, &tv))
    fprintf(stderr, "CLOCK_PROCESS_CPUTIME_ID not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_PROCESS_CPUTIME_ID resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_PROCESS_CPUTIME_ID resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_THREAD_CPUTIME_ID, &tv))
    fprintf(stderr, "CLOCK_THREAD_CPUTIME_ID not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_THREAD_CPUTIME_ID resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_THREAD_CPUTIME_ID resolution = %lu nsec.\n", tv.tv_nsec);  
  return EXIT_SUCCESS;
}

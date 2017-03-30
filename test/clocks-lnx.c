#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  struct timespec tv;
  fprintf(stdout, "sizeof(time_t) = %zu, sizeof(clock_t) = %zu, CLOCKS_PER_SEC = %lu.\n", sizeof(time_t), sizeof(clock_t), CLOCKS_PER_SEC);
  if (clock_getres(CLOCK_REALTIME, &tv))
    fprintf(stderr, "CLOCK_REALTIME not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_REALTIME resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_REALTIME resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_REALTIME_COARSE, &tv))
    fprintf(stderr, "CLOCK_REALTIME_COARSE not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_REALTIME_COARSE resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_REALTIME_COARSE resolution = %lu nsec.\n", tv.tv_nsec);
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
  if (clock_getres(CLOCK_MONOTONIC_COARSE, &tv))
    fprintf(stderr, "CLOCK_MONOTONIC_COARSE not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_MONOTONIC_COARSE resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_MONOTONIC_COARSE resolution = %lu nsec.\n", tv.tv_nsec);
  if (clock_getres(CLOCK_BOOTTIME, &tv))
    fprintf(stderr, "CLOCK_BOOTTIME not supported!\n");
  else if (tv.tv_sec)
    fprintf(stdout, "CLOCK_BOOTTIME resolution = %lu sec %lu nsec.\n", tv.tv_sec, tv.tv_nsec);
  else
    fprintf(stdout, "CLOCK_BOOTTIME resolution = %lu nsec.\n", tv.tv_nsec);
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

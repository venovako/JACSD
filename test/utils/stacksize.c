#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>

static struct rlimit rl;
static size_t siz;
static int err;

int main(void) {
  (void)printf("getrlimit(RLIMIT_STACK) = %d\n",
               (err = getrlimit(RLIMIT_STACK, (struct rlimit*)memset(&rl, 0, sizeof(rl)))));
  if (err) {
    perror("getrlimit");
    return EXIT_FAILURE;
  }
  siz = (size_t)(rl.rlim_cur);
  (void)fprintf(stdout, "RLIMIT_STACK soft = %zu bytes.\n", siz);
  siz = (size_t)(rl.rlim_max);
  (void)fprintf(stdout, "RLIMIT_STACK hard = %zu bytes.\n", siz);
  return EXIT_SUCCESS;
}

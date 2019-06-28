#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
#include <dlfcn.h>
/* for dladdr() */

/* consider CLOCK_MONOTONIC_RAW */

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_enter(void *const this_fn, void *const call_site)
{
  /* TODO */
}

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_exit(void *const this_fn, void *const call_site)
{
  /* TODO */
}

#endif /* VN_TEST */

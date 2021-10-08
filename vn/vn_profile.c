#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
#ifdef VN_PROFILE
static pthread_mutex_t prof_lock = PTHREAD_MUTEX_INITIALIZER;
static void (*on_exit_ptr)(void) = (void (*)(void))NULL;
static void *bt_root = NULL;
static FILE *bt_file = (FILE*)NULL;
static FILE *st_file = (FILE*)NULL;
static __thread char file_name[20];
static __thread FILE *prof_file = (FILE*)NULL;

#ifndef VN_PROFILE_FLSIZE
#define VN_PROFILE_FLSIZE 1036
#endif /* !VN_PROFILE_FLSIZE */
static __thread char fl[VN_PROFILE_FLSIZE]; /* f:1023, l:11 */

static void VN_NO_PROF pflerror(const char *const f, const int l)
{
  const char *const e = strerror(errno);
  (void)snprintf(fl, VN_PROFILE_FLSIZE, "%s:%d", f, l);
  (void)fprintf(stderr, "%s: %s\n", fl, e);
}

static int VN_NO_PROF bt_comp(const void *a, const void *b)
{
  if (a == b)
    return 0;
  const uintptr_t A = (uintptr_t)(((const vn_addr_rec_t*)a)->addr);
  const uintptr_t B = (uintptr_t)(((const vn_addr_rec_t*)b)->addr);
  if (A < B)
    return -1;
  if (B < A)
    return 1;
  return 0;
}

static void VN_NO_PROF bt_action(const void *node, VISIT order, int level)
{
  vn_addr_rec_t *const ar = *(vn_addr_rec_t**)node;
  if ((order == postorder) || (order == leaf)) {
    const size_t sl = (size_t)(ar->fof + 1);
    ar->fof = ftell(st_file);
    if (fwrite(ar->sym, sizeof(char), sl, st_file) != sl) {
      pflerror(__FILE__, __LINE__);
      return;
    }
    if (fwrite(ar, (sizeof(*ar) - sizeof(ar->sym)), 1, bt_file) != 1)
      pflerror(__FILE__, __LINE__);
  }
}

static void VN_NO_PROF bt_destroy()
{
  while (bt_root) {
    vn_addr_rec_t *const p = *(vn_addr_rec_t**)bt_root;
    void *const r = tdelete(p, &bt_root, bt_comp);
    if (p) {
      free(p->sym);
      free(p);
    }
    if (!r) {
      pflerror(__FILE__, __LINE__);
      return;
    }
  }
}

/* It is assumed that atexit() handlers are called when exactly one thread of the process has remained running. */
static void VN_NO_PROF on_prog_exit()
{
  if (pthread_mutex_lock(&prof_lock)) {
    pflerror(__FILE__, __LINE__);
    return;
  }
  file_name[17] = 's';
  if (!(st_file = fopen(file_name, "wb"))) {
    pflerror(__FILE__, __LINE__);
    goto err;
  }
  file_name[17] = 'b';
  if (!(bt_file = fopen(file_name, "wb"))) {
    pflerror(__FILE__, __LINE__);
    goto err;
  }
  twalk(bt_root, bt_action);
 err:
  if (bt_file && fclose(bt_file))
    pflerror(__FILE__, __LINE__);
  if (st_file && fclose(st_file))
    pflerror(__FILE__, __LINE__);
  if (fflush((FILE*)NULL))
    pflerror(__FILE__, __LINE__);
  bt_destroy();
  if (pthread_mutex_unlock(&prof_lock))
    pflerror(__FILE__, __LINE__);
  if (pthread_mutex_destroy(&prof_lock))
    pflerror(__FILE__, __LINE__);
}

static int VN_NO_PROF bt_insert(void *const addr)
{
  if (pthread_mutex_lock(&prof_lock)) {
    pflerror(__FILE__, __LINE__);
    return 1;
  }
  vn_addr_rec_t **const node = (vn_addr_rec_t**)tsearch((const void*)&addr, &bt_root, bt_comp);
  if (pthread_mutex_unlock(&prof_lock))
    pflerror(__FILE__, __LINE__);
  if (!node) {
    pflerror(__FILE__, __LINE__);
    return 2;
  }
  if ((const void*)*node == (const void*)&addr) {
    Dl_info info;
    if (addr && !dladdr(addr, (Dl_info*)memset(&info, 0, sizeof(info)))) {
      pflerror(__FILE__, __LINE__);
      goto bt_err;
    }
    /* replace the node's value with a pointer to a dynamically allocated struct */
    vn_addr_rec_t *const ar = (vn_addr_rec_t*)malloc(sizeof(vn_addr_rec_t));
    if (!ar) {
      pflerror(__FILE__, __LINE__);
      goto bt_err;
    }
    ar->addr = addr;
    ar->off = (info.dli_saddr ? ((uintptr_t)addr - (uintptr_t)(info.dli_saddr)) : (uintptr_t)0u);
    ar->fof = (long)(info.dli_sname ? strlen(info.dli_sname) : (size_t)0u);
    if (!(ar->sym = (char*)malloc(ar->fof + 2))) {
      pflerror(__FILE__, __LINE__);
      goto bt_err;
    }
    (void)strcpy(ar->sym, (info.dli_sname ? info.dli_sname : ""));
    (ar->sym)[ar->fof] = '\n';
    (ar->sym)[ar->fof + 1] = '\0';
    if (pthread_mutex_lock(&prof_lock)) {
      free(ar->sym);
      pflerror(__FILE__, __LINE__);
      goto bt_err;
    }
    *node = ar;
    if (pthread_mutex_unlock(&prof_lock))
      pflerror(__FILE__, __LINE__);
  }
  return 0;
 bt_err:
  if (!tdelete(&addr, &bt_root, bt_comp))
    pflerror(__FILE__, __LINE__);
  return 3;
}

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_enter(void *const this_fn, void *const call_site)
{
  if (!prof_file) {
    if (snprintf(file_name, sizeof(file_name), "%016tx.pt", pthread_self()) <= 0) {
      pflerror(__FILE__, __LINE__);
      return;
    }
    if (!(prof_file = fopen(file_name, "wb"))) {
      pflerror(__FILE__, __LINE__);
      return;
    }
    if (pthread_mutex_lock(&prof_lock)) {
      pflerror(__FILE__, __LINE__);
      return;
    }
    if (!on_exit_ptr) {
      if (atexit(on_prog_exit))
        pflerror(__FILE__, __LINE__);
      else
        on_exit_ptr = on_prog_exit;
    }
    if (pthread_mutex_unlock(&prof_lock))
      pflerror(__FILE__, __LINE__);
  }

  if (bt_insert(this_fn))
    return;
  if (bt_insert(call_site))
    return;

  vn_prof_rec_t pr;
  pr.this_fn = this_fn;
  pr.call_site = call_site;
  struct timespec tv;
  if (clock_gettime(CLOCK_MONOTONIC_RAW, (struct timespec*)memset(&tv, 0, sizeof(tv))))
    pflerror(__FILE__, __LINE__);
  pr.tns = t2ns(&tv);

  if (fwrite(&pr, sizeof(pr), 1, prof_file) != 1)
    pflerror(__FILE__, __LINE__);
}

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_exit(void *const this_fn, void *const call_site)
{
  vn_prof_rec_t pr;
  pr.this_fn = this_fn;
  pr.call_site = NULL; /* a marker for func_exit */
  struct timespec tv;
  if (clock_gettime(CLOCK_MONOTONIC_RAW, (struct timespec*)memset(&tv, 0, sizeof(tv))))
    pflerror(__FILE__, __LINE__);
  pr.tns = t2ns(&tv);

  if (fwrite(&pr, sizeof(pr), 1, prof_file) != 1)
    pflerror(__FILE__, __LINE__);
}
#endif /* VN_PROFILE */
#endif /* ?VN_TEST */

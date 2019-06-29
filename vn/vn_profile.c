#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
#ifdef VN_PROFILE
static pthread_mutex_t prof_lock = PTHREAD_MUTEX_INITIALIZER;
static void (*on_exit)(void) = (void (*)(void))NULL;
static void *bt_root = NULL;
static FILE *bt_file = (FILE*)NULL;
static __thread char file_name[20];
static __thread FILE *prof_file = (FILE*)NULL;

static int VN_NO_PROF bt_comp(const void *a, const void *b)
{
  if (a == b)
    return 0;
  const ptrdiff_t d = ((const vn_addr_info_t*)a)->addr - ((const vn_addr_info_t*)b)->addr;
  if (d < 0)
    return -1;
  if (d > 0)
    return 1;
  return 0;
}

static void VN_NO_PROF bt_action(const void *node, VISIT order, int level)
{
  const vn_addr_info_t *const p = *(const vn_addr_info_t**)node;
  vn_addr_record_t ar;
  switch (order) {
  case postorder:
  case leaf:
    ar.addr = p->addr;
    ar.off = p->addr - p->info.dli_saddr;
    (void)memset(ar.sym, 0, sizeof(ar.sym));
    if (p->info.dli_sname)
      strncpy(ar.sym, p->info.dli_sname, sizeof(ar.sym))[sizeof(ar.sym)-1] = '\0';
    if (fwrite(&ar, sizeof(ar), 1, bt_file) != 1)
      perror("fwrite");
    break;
  default: /* do nothing */;
  }
}

static void VN_NO_PROF bt_destroy()
{
  while (bt_root) {
    vn_addr_info_t *const p = *(vn_addr_info_t**)bt_root;
    if (!tdelete(p, &bt_root, bt_comp))
      perror("tdelete");
    free(p);
  }
}

static void VN_NO_PROF on_prog_exit()
{
  /* IT IS ASSUMED THAT atexit() HANDLERS ARE CALLED WHEN EXACTLY ONE THREAD OF */
  /* THE PROCESS HAS REMAINED RUNNING; OTHERWISE, THIS IS UNSAFE AND DANGEROUS! */
  if (pthread_mutex_destroy(&prof_lock))
    perror("pthread_mutex_destroy");
  if (fflush((FILE*)NULL))
    perror("fflush");
  file_name[17] = 'b';
  if ((bt_file = fopen(file_name, "wb"))) {
    twalk(bt_root, bt_action);
    if (fclose(bt_file))
      perror("fclose");
  }
  else
    perror("fopen");
  bt_destroy();
}

static int VN_NO_PROF bt_insert(void *const addr)
{
  if (!addr)
    return 0;
  vn_addr_info_t ai;
  ai.addr = addr;
  if (pthread_mutex_lock(&prof_lock))
    perror("pthread_mutex_lock");
  vn_addr_info_t **const node = (vn_addr_info_t**)tsearch(&ai, &bt_root, bt_comp);
  if (pthread_mutex_unlock(&prof_lock))
    perror("pthread_mutex_unlock");
  if (!node) {
    perror("tsearch");
    return 1;
  }
  if (*node == &ai) {
    if (!dladdr(ai.addr, &(ai.info))) {
      perror("dladdr");
      goto bt_err;
    }
    /* replace the node's value with a pointer to a dynamically allocated copy of ai */
    vn_addr_info_t *const nai = (vn_addr_info_t*)malloc(sizeof(vn_addr_info_t));
    if (!nai) {
      perror("malloc");
      goto bt_err;
    }
    *nai = ai;
    if (pthread_mutex_lock(&prof_lock))
      perror("pthread_mutex_lock");
    *node = nai;
    if (pthread_mutex_unlock(&prof_lock))
      perror("pthread_mutex_unlock");
  }
  return 0;
 bt_err:
  (void)tdelete(&ai, &bt_root, bt_comp);
  return 3;
}

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_enter(void *const this_fn, void *const call_site)
{
  if (!prof_file) {
    if (snprintf(file_name, sizeof(file_name), "%016tx.pt", pthread_self()) <= 0) {
      perror("snprintf");
      return;
    }
    if (!(prof_file = fopen(file_name, "wb"))) {
      perror("fopen");
      return;
    }
    if (pthread_mutex_lock(&prof_lock))
      perror("pthread_mutex_lock");
    if (!on_exit) {
      if (atexit(on_prog_exit))
        perror("atexit");
      else
        on_exit = on_prog_exit;
    }
    if (pthread_mutex_unlock(&prof_lock))
      perror("pthread_mutex_unlock");
  }

  if (bt_insert(this_fn))
    return;
  if (bt_insert(call_site))
    return;

  vn_profile_record_t pr;
  pr.this_fn = this_fn;
  pr.call_site = call_site;
  if (clock_gettime(CLOCK_MONOTONIC_RAW, &(pr.tv))) {
    perror("clock_gettime");
    return;
  }
  if (fwrite(&pr, sizeof(pr), 1, prof_file) != 1) {
    perror("fwrite");
    return;
  }
}

VN_EXTERN_C void VN_NO_PROF __cyg_profile_func_exit(void *const this_fn, void *const call_site)
{
  assert(prof_file);
  vn_profile_record_t pr;
  pr.this_fn = this_fn;
  pr.call_site = NULL; /* a marker for func_exit */
  if (clock_gettime(CLOCK_MONOTONIC_RAW, &(pr.tv))) {
    perror("clock_gettime");
    return;
  }
  if (fwrite(&pr, sizeof(pr), 1, prof_file) != 1) {
    perror("fwrite");
    return;
  }
}
#endif /* VN_PROFILE */
#endif /* ?VN_TEST */

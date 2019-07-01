// Linux GCC  : gcc -std=gnu11 -D_GNU_SOURCE -O3 -march=native -I.. prof1.c -o prof1.exe -L../.. -lvn -lpthread -lm -ldl
// Linux ICC  : icc -std=gnu11 -D_GNU_SOURCE -O3 -xHost -I.. prof1.c -o prof1.exe -L../.. -lvn -lpthread -lm -ldl
// macOS clang: clang -O3 -march=native -integrated-as -I.. prof1.c -o prof1.exe -L../.. -lvn -lpthread -lm -ldl
#include "vn_lib.h"

static int bt_fd = -1;
static void *bt_mem = NULL;
static struct stat bt_stat;
static size_t bt_count = 0;

static int bt_cmp(const void *a, const void *b)
{
  if (a == b)
    return 0;
  const vn_addr_rec_t *const A = (const vn_addr_rec_t*)a;
  const vn_addr_rec_t *const B = (const vn_addr_rec_t*)b;
  const uintptr_t addrA = (uintptr_t)(A->addr);
  const uintptr_t addrB = (uintptr_t)(B->addr);
  if (addrA < addrB)
    return -1;
  if (addrB < addrA)
    return 1;
  return 0;
}

static const vn_addr_rec_t *get_addr(const void *const addr)
{
  return (const vn_addr_rec_t*)(addr ? bsearch((const void*)&addr, bt_mem, bt_count, sizeof(vn_addr_rec_t), bt_cmp) : NULL);
}

static void print_addr(const vn_addr_rec_t *const ar)
{
  if (ar) {
    (void)fprintf(stdout, "%p,\"%s", ar->addr, ar->sym);
    if (ar->off)
      (void)fprintf(stdout, "+%#tx", ar->off);
    (void)fprintf(stdout, "\"");
  }
  else
    (void)fprintf(stdout, "%p,\"\"", NULL);
}

static void print_addrs(const void *const this_fn, const void *const call_site)
{
  const vn_addr_rec_t *ar = get_addr(this_fn);
  print_addr(ar);
  (void)fprintf(stdout, ",");
  ar = get_addr(call_site);
  print_addr(ar);
  (void)fprintf(stdout, "\n");
}

static void print_times(const struct timespec *const a, const struct timespec *const b)
{
  const long long at = (a ? (a->tv_sec * 1000000000ll + a->tv_nsec) : 0ll);
  const long long bt = (b ? (b->tv_sec * 1000000000ll + b->tv_nsec) : 0ll);
  const long long ns = (b ? (bt - at) : 0ll);
  (void)fprintf(stdout, "%lld,%lld,%lld,", at, bt, ns);
}

static void print_call(const vn_prof_rec_t *const a, const vn_prof_rec_t *const b)
{
  print_times(&(a->tv), (b ? &(b->tv) : (const struct timespec*)NULL));
  print_addrs(a->this_fn, a->call_site);
}

int main(int argc, char *argv[])
{
  if (argc != 3) {
    (void)fprintf(stderr, "%s BT PT\n", argv[0]);
    return EXIT_FAILURE;
  }
  if ((bt_fd = open(argv[1], O_RDONLY)) < 0) {
    perror("open(BT)");
    return EXIT_FAILURE;
  }
  if (fstat(bt_fd, &bt_stat)) {
    perror("fstat");
    return EXIT_FAILURE;
  }
  bt_count = bt_stat.st_size / sizeof(vn_addr_rec_t);
  if (!(bt_mem = mmap(NULL, bt_stat.st_size, PROT_READ, (MAP_FILE | MAP_SHARED), bt_fd, 0))) {
    perror("mmap");
    return EXIT_FAILURE;
  }
  const int pt_fd = open(argv[2], O_RDONLY);
  if (pt_fd < 0) {
    perror("open(PT)");
    return EXIT_FAILURE;
  }
  (void)fprintf(stdout, "\"TS_ENTER\",\"TS_EXIT\",\"CALL_TIME_ns\",\"FUNC_ADDR\",\"FUNC_NAME\",\"CALL_SITE\",\"CALL_LOC\"\n");
  vn_varentry_t *sp = (vn_varentry_t*)NULL;
  vn_prof_rec_t pr;
  const size_t ps = sizeof(pr);
  while (read(pt_fd, &pr, ps) == ps) {
    if (pr.call_site) {
      // function enter
      sp = vn_varstack_push_ptr(sp, memcpy(malloc(ps), &pr, ps), true);
    }
    else {
      // function exit
      vn_prof_rec_t *const cr = vn_varentry_get_ptr(sp);
      if (cr->this_fn != pr.this_fn) {
        (void)fprintf(stderr, "CALLS NOT MATCHED!\n");
        return EXIT_FAILURE;
      }
      print_call(cr, &pr);
      sp = vn_varstack_pop(sp);
    }
  }
  while (sp) {
    print_call((vn_prof_rec_t*)vn_varentry_get_ptr(sp), (const vn_prof_rec_t*)NULL);
    sp = vn_varstack_pop(sp);
  }
  if (close(pt_fd)) {
    perror("close(PT)");
    return EXIT_FAILURE;
  }
  if (munmap(bt_mem, bt_stat.st_size)) {
    perror("munmap");
    return EXIT_FAILURE;
  }
  if (close(bt_fd)) {
    perror("close(BT)");
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

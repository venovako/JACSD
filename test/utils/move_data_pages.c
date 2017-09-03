#include "move_data_pages.h"

#include <numaif.h>

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

long move_data_pages(void *const addr, const size_t sz, const int to_node)
{
  if (to_node < 0)
    return -1L;
  if (!sz)
    return 0L;
  if (!addr)
    return -1L;
  const long page_sz = sysconf(_SC_PAGESIZE);
  if (page_sz <= 0L)
    return -1L;
  // find the first bit set in page_sz
  unsigned fbs = 0u;
  for (long i = 1L; i > 0L; i <<= 1u) {
    if (page_sz & i)
      break;
    ++fbs;
  }
  // should never happen
  if (fbs >= 63u)
    return -1L;
  const uintptr_t mask = (uintptr_t)(~0ULL << fbs);
  void *const algn_addr = (void*)((uintptr_t)addr & mask);
  const size_t algn_sz = sz + (size_t)((uintptr_t)addr & ~mask);

  const int pid = 0;
  const int flags = MPOL_MF_MOVE;
  int status = 0;
  const unsigned long count = (unsigned long)(((long)algn_sz + (page_sz - 1L)) / page_sz);
  unsigned char *page = algn_addr;

  for (unsigned long i = 0UL; i < count; ++i) {
    long ret = move_pages(pid, 1UL, (void**)&page, (const int*)NULL, &status, flags);
    if (ret < 0L)
      return ret;
    if (status < 0)
      return (long)status;
    if (status != to_node) {
      ret = move_pages(pid, 1UL, (void**)&page, &to_node, &status, flags);
      if (ret < 0L)
	return ret;
      if (status < 0)
	return (long)status;
    }
    page += page_sz;
  }

  return 0L;
}

void move_data_pages_(void *const addr, const size_t *const sz, const int *const to_node, long *const info)
{
  assert(sz);
  assert(to_node);
  assert(info);

  info = move_data_pages(addr, *sz, *to_node);
}

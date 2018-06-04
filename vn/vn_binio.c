#include "vn_lib.h"

static const size_t GiB = (1ull << 30u);

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
int vn_bopen_ro(const char *const fn, off_t *const sz)
{
  const int fd = (fn ? open(fn, O_RDONLY) : -2);
  if (fd >= 0) {
    if (sz) {
      struct stat buf;
      *sz = -1;
      if (fstat(fd, &buf) < 0)
        return -fd;
      *sz = buf.st_size;
    }
  }
  return fd;
}

int vn_bopen_rw(const char *const fn, off_t *const sz)
{
  const int fd = (fn ? open(fn, (O_RDWR | O_CREAT), (S_IRUSR | S_IWUSR)) : -2);
  if (fd >= 0) {
    if (sz) {
      if (*sz >= 0) {
        if (ftruncate(fd, *sz) < 0)
          return -fd;
        (void)fsync(fd);
      }
      else {
        struct stat buf;
        *sz = -1;
        if (fstat(fd, &buf) < 0)
          return -fd;
        *sz = buf.st_size;
      }
    }
  }
  return fd;
}

int vn_bclose(const int fd)
{
  return ((fd < 0) ? -2 : close(fd));
}

ssize_t vn_bwrite(const int fd, const void *buf, size_t nB, off_t off)
{
  if (fd < 0)
    return -2;
  if (!buf && nB)
    return -3;
  if (!nB)
    return 0;
  if (off < 0)
    return -5;

  const size_t qGiB = (nB >> 30u);
  ssize_t ret = 0;
  for (size_t i = 0u; i < qGiB; ++i) {
    const ssize_t ret1 = pwrite(fd, buf, GiB, off);
    if (!ret1)
      return -4;
    if (ret1 < 0)
      return ret1;
    ret += ret1;
    (void)fsync(fd);
    if (ret1 < GiB)
      return ret;
    if (ret1 > GiB)
      return -(int)ret1;
    buf = (const void*)(((const char*)buf) + ret1);
    off += ret1;
    nB -= ret1;
  }
  const ssize_t ret1 = pwrite(fd, buf, nB, off);
  if (!ret1)
    return -4;
  if (ret1 < 0)
    return ret1;
  ret += ret1;
  (void)fsync(fd);
  if (ret1 > nB)
    return -(int)ret1;
  return ret;
}

ssize_t vn_bread(const int fd, void *buf, size_t nB, off_t off)
{
  if (fd < 0)
    return -2;
  if (!buf && nB)
    return -3;
  if (!nB)
    return 0;
  if (off < 0)
    return -5;
  const size_t qGiB = (nB >> 30u);
  ssize_t ret = 0;
  for (size_t i = 0u; i < qGiB; ++i) {
    const ssize_t ret1 = pread(fd, buf, GiB, off);
    if (!ret1)
      return -4;
    if (ret1 < 0)
      return ret1;
    ret += ret1;
    if (ret1 < GiB)
      return ret;
    if (ret1 > GiB)
      return -(int)ret1;
    buf = (void*)(((char*)buf) + ret1);
    off += ret1;
    nB -= ret1;
  }
  const ssize_t ret1 = pread(fd, buf, nB, off);
  if (!ret1)
    return -4;
  if (ret1 < 0)
    return ret1;
  ret += ret1;
  if (ret1 > nB)
    return -(int)ret1;
  return ret;
}
#endif /* VN_TEST */

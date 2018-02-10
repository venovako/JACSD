#include "vn_lib.h"

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

ssize_t vn_bwrite(const int fd, const void *const buf, const size_t nB, const off_t off)
{
  if (fd < 0)
    return -2;
  if (!buf && nB)
    return -3;
  if (!nB)
    return 0;
  if (off < 0)
    return -5;
  const ssize_t ret = pwrite(fd, buf, nB, off);
  if (!ret)
    return -4;
  if (ret > 0)
    (void)fsync(fd);
  return ret;
}

ssize_t vn_bread(const int fd, void *const buf, const size_t nB, const off_t off)
{
  if (fd < 0)
    return -2;
  if (!buf && nB)
    return -3;
  if (!nB)
    return 0;
  if (off < 0)
    return -5;
  const ssize_t ret = pread(fd, buf, nB, off);
  if (!ret)
    return -4;
  return ret;
}
#endif /* VN_TEST */

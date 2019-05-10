#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  vn_lock_t lock;
  vn_lock_init(&lock, true);
  (void)printf("1st lock = %d\n", vn_lock(&lock));
  (void)printf("2nd lock = %d\n", vn_lock(&lock));
  (void)printf("1st unlock = %d\n", vn_unlock(&lock));
  (void)printf("3rd lock = %d\n", vn_lock(&lock));
  vn_lock_destroy(&lock);
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
void vn_lock_init(vn_lock_t *const lock, const bool recursive)
{
  VN_SYSP_CALL(lock);
  lock->mutex_type = (recursive ? PTHREAD_MUTEX_RECURSIVE : PTHREAD_MUTEX_ERRORCHECK);
  lock->count = 0;
  pthread_mutexattr_t attr;
  VN_SYSI_CALL(pthread_mutexattr_init(&attr));
  VN_SYSI_CALL(pthread_mutexattr_settype(&attr, lock->mutex_type));
  VN_SYSI_CALL(pthread_mutex_init(&(lock->mutex), &attr));
  VN_SYSI_CALL(pthread_mutexattr_destroy(&attr));
}

void vn_lock_destroy(vn_lock_t *const lock)
{
  sig_atomic_t c = vn_lock(lock);
  if (c <= 0)
    return;
  lock->count = -1;
  for (sig_atomic_t j, i = (c - 1); i >= 0; --i)
    if ((j = vn_unlock(lock)) != -1)
      VN_SYSI_CALL(j);
  while (true) {
    const int e = pthread_mutex_destroy(&(lock->mutex));
    switch (e) {
    case 0:
      return;
    case EBUSY:
      break;
    default:
      VN_SYSI_CALL(e);
    }
  }
}

sig_atomic_t vn_lock(vn_lock_t *const lock)
{
  if (!lock)
    return -2;
  int e = pthread_mutex_lock(&(lock->mutex));
  switch (e) {
  case 0:
    break;
  case EINVAL:
    return -3;
  case EDEADLK:
    return -4;
  default:
    VN_SYSI_CALL(e);
  }
  if (lock->count >= 0)
    return ++(lock->count);
  /* else this lock is about to die */
  if ((e = pthread_mutex_unlock(&(lock->mutex))))
    VN_SYSI_CALL(e);
  return -1;
}

sig_atomic_t vn_unlock(vn_lock_t *const lock)
{
  if (!lock)
    return -2;
  if (lock->count > 0)
    --(lock->count);
  else if (!(lock->count))
    return -3;
  const int e = pthread_mutex_unlock(&(lock->mutex));
  switch (e) {
  case 0:
    break;
  case EINVAL:
    if (lock->count >= 0)
      ++(lock->count);
    return -4;
  case EPERM:
    if (lock->count >= 0)
      ++(lock->count);
    return -5;
  default:
    if (lock->count >= 0)
      ++(lock->count);
    VN_SYSI_CALL(e);
  }
  return (lock->count);
}
#endif /* VN_TEST */

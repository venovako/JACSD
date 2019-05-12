#ifndef VN_LOCK_H
#define VN_LOCK_H

#ifndef VN_LIB_H
#error vn_lock.h not intended for direct inclusion
#endif /* !VN_LIB_H */

typedef struct {
  pthread_mutex_t mutex;
  int mutex_type;
  volatile sig_atomic_t count;
} vn_lock_t;

VN_EXTERN_C void vn_lock_init(vn_lock_t *const, const bool);
VN_EXTERN_C void vn_lock_destroy(vn_lock_t *const);
VN_EXTERN_C sig_atomic_t vn_lock(vn_lock_t *const);
VN_EXTERN_C sig_atomic_t vn_unlock(vn_lock_t *const);

#endif /* !VN_LOCK_H */

#ifndef VN_BINIO_H
#define VN_BINIO_H

#ifndef VN_LIB_H
#error vn_binio.h not intended for direct inclusion
#endif /* !VN_LIB_H */

VN_EXTERN_C int vn_bopen_ro(const char *const fn, off_t *const sz);
VN_EXTERN_C int vn_bopen_rw(const char *const fn, off_t *const sz);
VN_EXTERN_C int vn_bopen_wo(const char *const fn, off_t *const sz);
VN_EXTERN_C int vn_bclose(const int fd);
VN_EXTERN_C ssize_t vn_bwrite(const int fd, const void *buf, size_t nB, off_t off);
VN_EXTERN_C ssize_t vn_bread(const int fd, void *buf, size_t nB, off_t off);

#endif /* !VN_BINIO_H */

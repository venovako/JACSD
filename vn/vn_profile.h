#ifndef VN_PROFILE_H
#define VN_PROFILE_H

typedef struct {
  void *this_fn;
  void *call_site;
  struct timespec tv;
} vn_profile_record_t;

typedef struct {
  void *addr;
  Dl_info info;
} vn_addr_info_t;

typedef struct {
  void *addr;
  ptrdiff_t off;
  char sym[48]; /* min 32 */
} vn_addr_record_t;

#endif /* !VN_PROFILE_H */

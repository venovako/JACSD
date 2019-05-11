#ifndef VN_VARIANT_H
#define VN_VARIANT_H

#ifndef VN_LIB_H
#error vn_variant.h not intended for direct inclusion
#endif /* !VN_LIB_H */

typedef union {
  double d;
  unsigned long long ull;
  long long ll;
  char *s; /* string, owned or not */
  void *p; /* any data, owned or not */
} vn_variant_t;

#ifdef VN_VARIANT_TAG_BITS
#error VN_VARIANT_TAG_BITS already defined
#define VN_VARIANT_TAG_BITS 3
#endif /* ?VN_VARIANT_TAG_BITS */

#ifdef VN_VARIANT_TAG_MASK
#error VN_VARIANT_TAG_MASK already defined
#else /* !VN_VARIANT_TAG_MASK */
#define VN_VARIANT_TAG_MASK ((intptr_t)7)
#endif /* ?VN_VARIANT_TAG_MASK */

#ifdef VN_VARIANT_TAG_d
#error VN_VARIANT_TAG_d already defined
#else /* !VN_VARIANT_TAG_d */
#define VN_VARIANT_TAG_d 6
#endif /* ?VN_VARIANT_TAG_d */

#ifdef VN_VARIANT_TAG_ull
#error VN_VARIANT_TAG_ull already defined
#else /* !VN_VARIANT_TAG_ull */
#define VN_VARIANT_TAG_ull 5
#endif /* ?VN_VARIANT_TAG_ull */

#ifdef VN_VARIANT_TAG_ll
#error VN_VARIANT_TAG_ll already defined
#else /* !VN_VARIANT_TAG_ll */
#define VN_VARIANT_TAG_ll 4
#endif /* ?VN_VARIANT_TAG_ll */

#ifdef VN_VARIANT_TAG_S
#error VN_VARIANT_TAG_S already defined
#else /* !VN_VARIANT_TAG_S */
#define VN_VARIANT_TAG_S 3
#endif /* ?VN_VARIANT_TAG_S */

#ifdef VN_VARIANT_TAG_s
#error VN_VARIANT_TAG_s already defined
#else /* !VN_VARIANT_TAG_s */
#define VN_VARIANT_TAG_s 2
#endif /* ?VN_VARIANT_TAG_s */

#ifdef VN_VARIANT_TAG_P
#error VN_VARIANT_TAG_P already defined
#else /* !VN_VARIANT_TAG_P */
#define VN_VARIANT_TAG_P 1
#endif /* ?VN_VARIANT_TAG_P */

#ifdef VN_VARIANT_TAG_p
#error VN_VARIANT_TAG_p already defined
#else /* !VN_VARIANT_TAG_p */
#define VN_VARIANT_TAG_p 0
#endif /* ?VN_VARIANT_TAG_p */

typedef struct _vn_varentry_t {
  vn_variant_t v;
  struct _vn_varentry_t *p;
} vn_varentry_t;

VN_EXTERN_C vn_varentry_t *vn_varentry_get_p(const vn_varentry_t *const);
VN_EXTERN_C int vn_varentry_get_t(const vn_varentry_t *const);
VN_EXTERN_C int vn_varentry_get_p_t(const vn_varentry_t *const, vn_varentry_t **const, int *const);

VN_EXTERN_C int vn_varentry_set_p(vn_varentry_t *const, vn_varentry_t *const);
VN_EXTERN_C int vn_varentry_set_t(vn_varentry_t *const, const int);

VN_EXTERN_C int vn_varentry_init(vn_varentry_t *const);
VN_EXTERN_C int vn_varentry_destroy(vn_varentry_t *const);

VN_EXTERN_C int vn_varentry_set_ptr(vn_varentry_t *const, void *const, const bool);
VN_EXTERN_C int vn_varentry_set_str(vn_varentry_t *const, const char *const, const int);
VN_EXTERN_C int vn_varentry_set_int(vn_varentry_t *const, const long long);
VN_EXTERN_C int vn_varentry_set_nat(vn_varentry_t *const, const unsigned long long);
VN_EXTERN_C int vn_varentry_set_flt(vn_varentry_t *const, const double);

VN_EXTERN_C void *vn_varentry_get_ptr(const vn_varentry_t *const);
VN_EXTERN_C char *vn_varentry_get_str(const vn_varentry_t *const);
VN_EXTERN_C long long vn_varentry_get_int(const vn_varentry_t *const);
VN_EXTERN_C unsigned long long vn_varentry_get_nat(const vn_varentry_t *const);
VN_EXTERN_C double vn_varentry_get_flt(const vn_varentry_t *const);

VN_EXTERN_C int vn_varentry_print(const vn_varentry_t *const, FILE *const);

VN_EXTERN_C vn_varentry_t *vn_varstack_push_ptr(vn_varentry_t *const, void *const, const bool);
VN_EXTERN_C vn_varentry_t *vn_varstack_push_str(vn_varentry_t *const, const char *const, const int);
VN_EXTERN_C vn_varentry_t *vn_varstack_push_int(vn_varentry_t *const, const long long);
VN_EXTERN_C vn_varentry_t *vn_varstack_push_nat(vn_varentry_t *const, const unsigned long long);
VN_EXTERN_C vn_varentry_t *vn_varstack_push_flt(vn_varentry_t *const, const double);

VN_EXTERN_C vn_varentry_t *vn_varstack_pop(vn_varentry_t *const);

VN_EXTERN_C int vn_varstack_print(const vn_varentry_t *const, const char, FILE *const);
#endif /* !VN_VARIANT_H */

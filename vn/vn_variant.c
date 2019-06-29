#include "vn_lib.h"

#ifdef VN_TEST
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  vn_varentry_t *e = (vn_varentry_t*)NULL;
  e = vn_varstack_push_ptr(e, NULL, false);
  e = vn_varstack_push_str(e, "stack \"stack\"", 0);
  e = vn_varstack_push_int(e, 17);
  e = vn_varstack_push_nat(e, 19u);
  e = vn_varstack_push_flt(e, M_PI);
  (void)printf("\nprint = %d\n", vn_varstack_print(e, ','/*'\n'*/, stdout));
  while ((e = vn_varstack_pop(e)));
  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
vn_varentry_t *vn_varentry_get_p(const vn_varentry_t *const e)
{
  return (e ? (vn_varentry_t*)((intptr_t)(e->p) & ~VN_VARIANT_TAG_MASK) : (vn_varentry_t*)NULL);
}

int vn_varentry_get_t(const vn_varentry_t *const e)
{
  return (int)(e ? ((intptr_t)(e->p) & VN_VARIANT_TAG_MASK) : VN_VARIANT_TAG_MASK);
}

int vn_varentry_get_p_t(const vn_varentry_t *const e, vn_varentry_t **const p, int *const t)
{
  if (p)
    *p = vn_varentry_get_p(e);
  if (t)
    *t = vn_varentry_get_t(e);
  return (e ? 0 : -1);
}

int vn_varentry_set_p(vn_varentry_t *const e, vn_varentry_t *const p)
{
  const intptr_t t = (intptr_t)vn_varentry_get_t(e);
  if (t == VN_VARIANT_TAG_MASK)
    return -1;
  e->p = (vn_varentry_t*)(((intptr_t)p & ~VN_VARIANT_TAG_MASK) | t);
  return 0;
}

int vn_varentry_set_t(vn_varentry_t *const e, const int t)
{
  if (!e)
    return -1;
  if (t < 0)
    return -2;
  const intptr_t t_ = (intptr_t)t;
  if (t_ > VN_VARIANT_TAG_MASK)
    return -2;
  e->p = (vn_varentry_t*)(((intptr_t)(e->p) & ~VN_VARIANT_TAG_MASK) | t_);
  return 0;
}

int vn_varentry_init(vn_varentry_t *const e)
{
  if (!e)
    return -1;
  (void)memset(e, 0, sizeof(*e));
  return 0;
}

int vn_varentry_destroy(vn_varentry_t *const e)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
  default:
    return vn_varentry_init(e);
  }
}

int vn_varentry_set_ptr(vn_varentry_t *const e, void *const p, const bool o)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
    break;
  case (int)VN_VARIANT_TAG_MASK:
    return -1;
  }
  (e->v).p = p;
  return vn_varentry_set_t(e, (o ? VN_VARIANT_TAG_P : VN_VARIANT_TAG_p));
}

int vn_varentry_set_str(vn_varentry_t *const e, const char *const s, const int oc)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
    break;
  case (int)VN_VARIANT_TAG_MASK:
    return -1;
  }
  char *s_ = (char*)s;
  const int t = (oc ? VN_VARIANT_TAG_S : VN_VARIANT_TAG_s);
  if ((oc < 0) && s) {
    if (!(s_ = (char*)malloc((strlen(s) + 1u) * sizeof(char))))
      return 1;
    (void)strcpy(s_, s);
  }
  (e->v).s = s_;
  return vn_varentry_set_t(e, t);
}

int vn_varentry_set_int(vn_varentry_t *const e, const long long ll)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
    break;
  case (int)VN_VARIANT_TAG_MASK:
    return -1;
  }
  (e->v).ll = ll;
  return vn_varentry_set_t(e, VN_VARIANT_TAG_ll);
}

int vn_varentry_set_nat(vn_varentry_t *const e, const unsigned long long ull)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
    break;
  case (int)VN_VARIANT_TAG_MASK:
    return -1;
  }
  (e->v).ull = ull;
  return vn_varentry_set_t(e, VN_VARIANT_TAG_ull);
}

int vn_varentry_set_flt(vn_varentry_t *const e, const double d)
{
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_S:
    free((e->v).s);
    break;
  case (int)VN_VARIANT_TAG_MASK:
    return -1;
  }
  (e->v).d = d;
  return vn_varentry_set_t(e, VN_VARIANT_TAG_d);
}

void *vn_varentry_get_ptr(const vn_varentry_t *const e)
{
  return (e ? (e->v).p : NULL);
}

char *vn_varentry_get_str(const vn_varentry_t *const e)
{
  return (e ? (e->v).s : (char*)NULL);
}

long long vn_varentry_get_int(const vn_varentry_t *const e)
{
  return (e ? (e->v).ll : 0ll);
}

unsigned long long vn_varentry_get_nat(const vn_varentry_t *const e)
{
  return (e ? (e->v).ull : 0ull);
}

double vn_varentry_get_flt(const vn_varentry_t *const e)
{
  return (e ? (e->v).d : 0.0);
}

int vn_varentry_print(const vn_varentry_t *const e, FILE *const f)
{
  if (!f)
    return -2;
  int ret = -1;
  switch (vn_varentry_get_t(e)) {
  case VN_VARIANT_TAG_P:
  case VN_VARIANT_TAG_p:
    ret = fprintf(f, "%p", (e->v).p);
    break;
  case VN_VARIANT_TAG_S:
  case VN_VARIANT_TAG_s:
    if ((e->v).s) {
#ifdef VN_NO_STR_CSV_QUOTE
      ret = fprintf(f, "%s", (e->v).s);
#else /* !VN_NO_STR_CSV_QUOTE */
      if (fprintf(f, "\"") != 1)
        return -1;
      ret = 1;
      for (const char *s = (e->v).s; *s; ++s) {
        int r = 0;
        if (*s == '\"') {
          r = fprintf(f, "\"\"");
          if (r != 2)
            return -1;
        }
        else {
          r = fprintf(f, "%c", *s);
          if (r < 1)
            return -1;
        }
        ret += r;
      }
      if (fprintf(f, "\"") != 1)
        return -1;
      ++ret;
#endif /* ?VN_NO_STR_CSV_QUOTE */
    }
    break;
  case VN_VARIANT_TAG_ll:
    ret = fprintf(f, "%lld", (e->v).ll);
    break;
  case VN_VARIANT_TAG_ull:
    ret = fprintf(f, "%llu", (e->v).ull);
    break;
  case VN_VARIANT_TAG_d:
    ret = fprintf(f, "%#.17e", (e->v).d);
    break;
  }
  return ret;
}

vn_varentry_t *vn_varstack_push_ptr(vn_varentry_t *const e, void *const p, const bool o)
{
  vn_varentry_t *const h = calloc(1u, sizeof(vn_varentry_t));
  (void)vn_varentry_set_ptr(h, p, o);
  if (e)
    (void)vn_varentry_set_p(h, e);
  return h;
}

vn_varentry_t *vn_varstack_push_str(vn_varentry_t *const e, const char *const s, const int oc)
{
  vn_varentry_t *const h = calloc(1u, sizeof(vn_varentry_t));
  if (vn_varentry_set_str(h, s, oc)) {
    free(h);
    return (vn_varentry_t*)NULL;
  }
  if (e)
    (void)vn_varentry_set_p(h, e);
  return h;
}

vn_varentry_t *vn_varstack_push_int(vn_varentry_t *const e, const long long ll)
{
  vn_varentry_t *const h = calloc(1u, sizeof(vn_varentry_t));
  (void)vn_varentry_set_int(h, ll);
  if (e)
    (void)vn_varentry_set_p(h, e);
  return h;
}

vn_varentry_t *vn_varstack_push_nat(vn_varentry_t *const e, const unsigned long long ull)
{
  vn_varentry_t *const h = calloc(1u, sizeof(vn_varentry_t));
  (void)vn_varentry_set_nat(h, ull);
  if (e)
    (void)vn_varentry_set_p(h, e);
  return h;
}

vn_varentry_t *vn_varstack_push_flt(vn_varentry_t *const e, const double d)
{
  vn_varentry_t *const h = calloc(1u, sizeof(vn_varentry_t));
  (void)vn_varentry_set_flt(h, d);
  if (e)
    (void)vn_varentry_set_p(h, e);
  return h;
}

vn_varentry_t *vn_varstack_pop(vn_varentry_t *const e)
{
  vn_varentry_t *const p = vn_varentry_get_p(e);
  (void)vn_varentry_destroy(e);
  free(e);
  return p;
}

int vn_varstack_print(const vn_varentry_t *const e, const char d, FILE *const f)
{
  if (!f)
    return -2;
  int tot = 0;
  for (const vn_varentry_t *h = e; h; h = (const vn_varentry_t*)vn_varentry_get_p(h)) {
    int ret = vn_varentry_print(h, f);
    if (ret <= 0)
      return ret;
    tot += ret;
    if (vn_varentry_get_p(h))
      ret = fprintf(f, "%c", d);
    else
      ret = fflush(f);
    if (ret < 0)
      return ret;
    tot += ret;
  }
  return tot;
}
#endif /* ?VN_TEST */

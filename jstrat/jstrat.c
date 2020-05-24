#include "jstrat.h"
#include "jstrat_ME.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif /* !WIN32_LEAN_AND_MEAN */
#ifndef _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE
#endif /* !_CRT_NONSTDC_NO_DEPRECATE */
#ifndef _CRT_NONSTDC_NO_WARNINGS
#define _CRT_NONSTDC_NO_WARNINGS
#endif /* !_CRT_NONSTDC_NO_WARNINGS */
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif /* !_CRT_SECURE_NO_DEPRECATE */
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif /* !_CRT_SECURE_NO_WARNINGS */
#ifndef _USE_MATH_DEFINES
#define _USE_MATH_DEFINES
#endif /* !_USE_MATH_DEFINES */
#endif /* _WIN32 */

#ifdef __cplusplus
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#else /* !__cplusplus */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif /* ?__cplusplus */

#ifdef USE_INTEL
#pragma optimize("", off)
#endif /* USE_INTEL */

// to avoid pulling in a compiler-dependent memset as a dependency
static void *mset(void *const p, const unsigned char b, const size_t s)
{
  if (p)
    for (size_t i = 0u; i < s; ++i)
      ((unsigned char*)p)[i] = b;
  return p;
}

// to avoid pulling in a compiler-dependent memcpy as a dependency
static void *mcpy(void *const dst, const void *const src, const size_t szb)
{
  if (dst && src && (dst != src))
    for (size_t i = 0u; i < szb; ++i)
      ((unsigned char*)dst)[i] = ((const unsigned char*)src)[i];
  return dst;
}

#ifdef USE_INTEL
#pragma optimize("", on)
#endif /* USE_INTEL */

#ifdef VN_TEST
static integer test_rolcyc(const integer id, const integer n)
{
  jstrat_common js;
  const integer ret = jstrat_init(&js, id, n);
  if (ret <= 0)
    return ret;
  integer arr[2];
  (void)fprintf(stdout, "{\n\t");
  for (integer stp = 0; stp < ret; ++stp) {
    const integer r = jstrat_next(&js, arr);
    if (r != 1)
      return -iabs(r);
    (void)fprintf(stdout, "{%3ld,%3ld},", arr[0], arr[1]);
  }
  (void)fprintf(stdout, "\b \n}\n");
  return ret;
}

static integer test_maneb2(const integer id, const integer n)
{
  jstrat_common js;
  const integer ret = jstrat_init(&js, id, n);
  if (ret <= 0)
    return ret;
  (void)fprintf(stdout, "{\n\t");
  const integer n_2 = n >> 1;
  if (id & (integer)1) { // comm
    integer arr[n_2][2][2];
    for (integer stp = 0; stp < ret; ++stp) {
      const integer r = jstrat_next(&js, (integer*)arr);
      if (r != n_2)
        return -iabs(r);
      (void)fprintf(stdout, "{\n\t\t");
      for (integer p = 0; p < r; ++p)
        (void)fprintf(stdout, "{%3ld,%3ld}:[%3ld,%3ld],", arr[p][0][0], arr[p][0][1], arr[p][1][0], arr[p][1][1]);
      (void)fprintf(stdout, "\b \n\t},");
    }
    (void)fprintf(stdout, "\b \n}\n");
  }
  else { // no comm
    integer arr[n_2][2];
    for (integer stp = 0; stp < ret; ++stp) {
      const integer r = jstrat_next(&js, (integer*)arr);
      if (r != n_2)
        return -iabs(r);
      (void)fprintf(stdout, "{\n\t\t");
      for (integer p = 0; p < r; ++p)
        (void)fprintf(stdout, "{%3ld,%3ld},", arr[p][0], arr[p][1]);
      (void)fprintf(stdout, "\b \n\t},");
    }
    (void)fprintf(stdout, "\b \n}\n");
  }
  return ret;
}

static integer test_modmod(const integer id, const integer n)
{
  jstrat_common js;
  const integer ret = jstrat_init(&js, id, n);
  if (ret <= 0)
    return ret;
  (void)fprintf(stdout, "{\n\t");
  const integer n_2 = n >> 1;
  if (id & (integer)1) { // comm
    int arr[n_2][2][2][2];
    for (integer stp = 0; stp < ret; ++stp) {
      const integer r = -jstrat_next(&js, (integer*)arr);
      if (r != n_2)
        return -iabs(r);
      (void)fprintf(stdout, "{\n\t\t");
      for (integer p = 0; p < r; ++p)
        (void)fprintf(stdout, "{%3d,%3d}:[%3d,%3d],", arr[p][0][0][0], arr[p][0][1][0], arr[p][1][0][0], arr[p][1][1][0]);
      (void)fprintf(stdout, "\b \n\t},");
    }
    (void)fprintf(stdout, "\b \n}\n");
  }
  else { // no comm
    int arr[n_2][2][2];
    for (integer stp = 0; stp < ret; ++stp) {
      const integer r = -jstrat_next(&js, (integer*)arr);
      if (r != n_2)
        return -iabs(r);
      (void)fprintf(stdout, "{\n\t\t");
      for (integer p = 0; p < r; ++p)
        (void)fprintf(stdout, "{%3d,%3d},", arr[p][0][0], arr[p][1][0]);
      (void)fprintf(stdout, "\b \n\t},");
    }
    (void)fprintf(stdout, "\b \n}\n");
  }
  return ret;
}

int main(int argc, char *argv[])
{
  if (argc != 3) {
    (void)fprintf(stderr, "%s id n\n", argv[0]);
    return EXIT_FAILURE;
  }
  const integer id = atol(argv[1]);
  const integer n = atol(argv[2]);
  if (n <= 1)
    return EXIT_FAILURE;
  integer ret = 0;
  switch (id & ~(integer)1) {
  case 0:
    ret = test_rolcyc(id, n);
    break;
  case 2:
    ret = test_maneb2(id, n);
    break;
  case 4:
    ret = test_modmod(id, n);
    break;
  default:
    return EXIT_FAILURE;
  }
  (void)fprintf(((ret <= 0) ? stderr : stdout), "ret = %ld\n", ret);
  return ((ret <= 0) ? EXIT_FAILURE : EXIT_SUCCESS);
}
#endif /* VN_TEST */

static integer *me_dup(const integer n1, const integer *const tbl)
{
  if (!tbl)
    return (integer*)NULL;
  if (n1 < 2)
    return (integer*)NULL;
  if (n1 & 1)
    return (integer*)NULL;

  const integer s1 = (n1 - 1);
  const integer p1 = (n1 >> 1);
  const integer n2 = (n1 << 1);
  // const integer s2 = (n2 - 1);
  const integer p2 = (n2 >> 1);

  integer (*const tbl2)[2] = (integer (*)[2])malloc(((size_t)n2 - 1u) * (size_t)n2 * sizeof(integer));
  if (!tbl2)
    return (integer*)NULL;
  const integer (*const tbl1)[2] = (const integer (*)[2])tbl;

  // (NE,SW); (NW,SE)
  for (integer p = 0; p < s1; ++p) {
    const integer q = (p << 1);
    for (integer a = 0; a < p1; ++a) {
      const integer b = (a << 1);
      const integer k = (p * p1 + a);
      const integer l = (q * p2 + b);
      const integer m = ((q + 1) * p2 + b);
      const integer i = tbl1[k][0];
      const integer j = tbl1[k][1];
      const integer i2 = (i << 1);
      const integer j2 = (j << 1);
      // NE
      tbl2[l][0] = i2;
      tbl2[l][1] = (j2 + 1);
      // SW
      tbl2[l + 1][0] = (i2 + 1);
      tbl2[l + 1][1] = j2;
      // NW
      tbl2[m][0] = i2;
      tbl2[m][1] = j2;
      // SE
      tbl2[m + 1][0] = (i2 + 1);
      tbl2[m + 1][1] = (j2 + 1);
    }
  }
  // super-diag
  const integer d = ((n2 - 2) * p2);
  for (integer i = 0; i < p2; ++i)
    tbl2[d + i][1] = ((tbl2[d + i][0] = (i << 1)) + 1);

  return &(tbl2[0][0]);
}

static integer me_base(const integer n, integer *const p2)
{
  if (!p2)
    return -2;
  *p2 = 0;

  if (n < 2)
    return -1;
  if (n & (integer)1)
    return -1;

  for (integer i = (n >> 1); i; i >>= 1) {
    if (i & (integer)1)
      return (i << 1);
    ++*p2;
  }

  return 0;
}

static integer *me_p2(const integer n)
{
  integer p2 = 0;
  const integer b = me_base(n, &p2);
  if (b <= 0)
    return (integer*)NULL;

  const integer *bp = (const integer*)NULL;
  if (b == 2)
    bp = &(ME0002[0][0][0]);
  else if (b == 6)
    bp = &(ME0006[0][0][0]);
  else if (b == 10)
    bp = &(ME0010[0][0][0]);
  else if (b == 14)
    bp = &(ME0014[0][0][0]);
  else if (b == 18)
    bp = &(ME0018[0][0][0]);
  else if (b == 22)
    bp = &(ME0022[0][0][0]);
  else if (b == 26)
    bp = &(ME0026[0][0][0]);
  else if (b == 30)
    bp = &(ME0030[0][0][0]);
  else if (b == 34)
    bp = &(ME0034[0][0][0]);
  else if (b == 38)
    bp = &(ME0038[0][0][0]);
  else if (b == 42)
    bp = &(ME0042[0][0][0]);
  else // no such base table
    return (integer*)NULL;

  const size_t bsz = (bp ? (((size_t)b - 1u) * (size_t)b * sizeof(integer)) : (size_t)0u);
  integer *cur = (integer*)(bsz ? malloc(bsz) : NULL);
  if (!cur)
    return cur;
  (void)mcpy(cur, bp, bsz);
  integer *prev = (integer*)NULL;

  for (integer i = 0; i < p2; ++i) {
    prev = cur;
    cur = me_dup((b << i), cur);
    free(prev);
    if (!cur)
      break;
  }

  return cur;
}

integer jstrat_init(jstrat_common *const js, const integer id, const integer n)
{
  if (!js)
    return -1;
  if (id < 0)
    return -2;
  if (n < 2)
    return -3;
  integer info = 0;

  if (!(id & ~(integer)1)) { // row/col-cyclic
    (void)mset(js, 0u, sizeof(jstrat_rolcyc));
    if (n & (integer)1) // n odd
      info = n * ((n - 1) >> 1);
    else // n even
      info = (n >> 1) * (n - 1);
  }
  else if ((id & ~(integer)1) == 2) { // Mantharam-Eberlein
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)mset(js, 0u, sizeof(jstrat_maneb2));
    const integer *const cur = me_p2(n);
    if (!cur)
      return -3;
    me2->nxt = me2->tbl = cur;
    info = n - 1;
  }
  else if ((id & ~(integer)1) == 4) { // modified modulus
    (void)mset(js, 0u, sizeof(jstrat_modmod));
    if (n & (integer)1) // n odd
      return -3;
    info = n;
  }
  else
    return -2;

  js->id = id;
  js->n = n;
  return info;
}

integer jstrat_next(jstrat_common *const js, integer *const arr)
{
  if (!js)
    return -1;
  if (!arr)
    return -2;
  integer info = 0;

  if (!(js->id)) { // row-cyclic
    jstrat_rolcyc *const row = (jstrat_rolcyc*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    if (++(row->q) >= (row->n)) {
      if (++(row->p) >= (row->n - 1)) {
        row->p = 0;
        row->q = 1;
      }
      else
        row->q = row->p + 1;
    }
    pairs[0][0] = row->p;
    pairs[0][1] = row->q;

    info = 1;
  }
  else if (js->id == 1) { // column-cyclic
    jstrat_rolcyc *const col = (jstrat_rolcyc*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    if (++(col->p) >= (col->q)) {
      if (++(col->q) >= (col->n)) {
        col->p = 0;
        col->q = 1;
      }
      else
        col->p = 0;
    }

    pairs[0][0] = col->p;
    pairs[0][1] = col->q;

    info = 1;
  }
  else if (js->id == 2) { // Mantharam-Eberlein, no comm
    // [RANK][p/q]
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    const integer n_2 = me2->n >> 1;
    for (integer i = 0; i < n_2; ++i) {
      for (integer k = 0; k < 2; ++k) {
        pairs[i][k] = *(me2->nxt);
        ++(me2->nxt);
      }
    }

    if (++(me2->stp) >= (me2->n - 1)) {
      me2->stp = 0;
      ++(me2->swp);
      me2->nxt = me2->tbl;
    }

    info = n_2;
  }
  else if (js->id == 3) { // Mantharam-Eberlein
    // [RANK][0=PAIR,1=COMM][p/q]
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    integer (*const pairs)[2][2] = (integer (*)[2][2])arr;

    const integer *const cur = me2->nxt;
    const integer n_2 = me2->n >> 1;
    for (integer i = 0; i < n_2; ++i) {
      for (integer k = 0; k < 2; ++k) {
        pairs[i][0][k] = *(me2->nxt);
        ++(me2->nxt);
      }
    }

    if (++(me2->stp) >= (me2->n - 1)) {
      me2->stp = 0;
      ++(me2->swp);
      me2->nxt = me2->tbl;
    }

    // communication
    for (integer i = 0; i < me2->n; ++i) {
      for (integer j = 0; j < me2->n; ++j) {
        // cur[i] sent to nxt[j]
        if (cur[i] == (me2->nxt)[j]) {
          const integer k_1 = i & (integer)1;
          const integer k_2 = i >> 1;
          const integer l_1 = j & (integer)1;
          const integer l_2 = j >> 1;
          const integer snd = l_2 + 1;
          pairs[k_2][1][k_1] = (l_1 ? snd : -snd);
        }
      }
    }

    info = n_2;
  }
  else if (js->id == 4) { // modified modulus, no comm
    // [RANK][p/q][0=PAIR,1=SHADOW]
    jstrat_modmod *const mom = (jstrat_modmod*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;
    int (*const ij)[2][2] = (int (*)[2][2])pairs;

    const int _n = (int)(mom->n);
    const int _n_2 = _n >> 1;
    const int _n1 = _n - 1;

    if (!(mom->stp) && !(mom->swp)) { // init
      for (int r = 0; r < _n_2; ++r) {
        ij[r][0][1] = ij[r][0][0] = r;
        ij[r][1][1] = ij[r][1][0] = _n1 - r;
      }
    }
    else { // step
      for (int r = 0; r < _n_2; ++r) {
        if ((ij[r][0][1] + ij[r][1][1]) >= _n1) {
          if (++(ij[r][0][1]) == ij[r][1][1])
            ij[r][1][1] = (ij[r][0][1] -= _n_2);
          ij[r][0][0] = ij[r][0][1];
        }
        else
          ij[r][1][0] = ++(ij[r][1][1]);
      }
    }

    if (++(mom->stp) >= mom->n) {
      mom->stp = 0;
      ++(mom->swp);
    }
    info = -_n_2;
  }
  else if (js->id == 5) { // modified modulus
    // [RANK][0=PAIR,1=COMM][p/q][0=PAIR,1=SHADOW]
    jstrat_modmod *const mom = (jstrat_modmod*)js;
    integer (*const pairs)[2][2] = (integer (*)[2][2])arr;
    int (*const ij)[2][2][2] = (int (*)[2][2][2])pairs;

    const int _n = (int)(mom->n);
    const int _n_2 = _n >> 1;
    const int _n1 = _n - 1;

    if (!(mom->stp) && !(mom->swp)) { // init
      for (int r = 0; r < _n_2; ++r) {
        ij[r][0][0][1] = ij[r][0][0][0] = r;
        ij[r][0][1][1] = ij[r][0][1][0] = _n1 - r;
      }
    }
    else { // step
      for (int r = 0; r < _n_2; ++r) {
        if ((ij[r][0][0][1] + ij[r][0][1][1]) >= _n1) {
          if (++(ij[r][0][0][1]) == ij[r][0][1][1])
            ij[r][0][1][1] = (ij[r][0][0][1] -= _n_2);
          ij[r][0][0][0] = ij[r][0][0][1];
        }
        else
          ij[r][0][1][0] = ++(ij[r][0][1][1]);
      }
    }

    for (int r = 0; r < _n_2; ++r) {
      ij[r][1][0][0] = ij[r][0][0][0];
      ij[r][1][0][1] = ij[r][0][0][1];
      ij[r][1][1][0] = ij[r][0][1][0];
      ij[r][1][1][1] = ij[r][0][1][1];
    }

    for (int r = 0; r < _n_2; ++r) {
      if ((ij[r][1][0][1] + ij[r][1][1][1]) >= _n1) {
        if (++(ij[r][1][0][1]) == ij[r][1][1][1])
          ij[r][1][1][1] = (ij[r][1][0][1] -= _n_2);
        ij[r][1][0][0] = ij[r][1][0][1];
      }
      else
        ij[r][1][1][0] = ++(ij[r][1][1][1]);
    }

    for (int r = 0; r < _n_2; ++r) {
      ij[r][1][0][1] = ij[r][1][0][0];
      ij[r][1][0][0] = 0;
      ij[r][1][1][1] = ij[r][1][1][0];
      ij[r][1][1][0] = 0;
    }

    if (++(mom->stp) >= mom->n) {
      mom->stp = 0;
      ++(mom->swp);
    }
    info = -_n_2;

    // communication
    for (int i = 0; i < _n; ++i) {
      for (int j = 0; j < _n; ++j) {
        const int Is1 = i >> 1;
        const int Ia1 = i & (integer)1;
        const int Js1 = j >> 1;
        const int Ja1 = j & (integer)1;
        const int cur = ij[Is1][0][Ia1][0];
        const int nxt = ij[Js1][1][Ja1][1];
        // cur sent to nxt
        if (cur == nxt) {
          const int k_1 = Ia1;
          const int k_2 = Is1;
          const int l_1 = Ja1;
          const int l_2 = Js1;
          const int snd = l_2 + 1;
          ij[k_2][1][k_1][0] = (l_1 ? snd : -snd);
        }
      }
    }

    for (int r = 0; r < _n_2; ++r)
      ij[r][1][1][1] = ij[r][1][0][1] = 0;
  }
  else
    info = 0;

  return info;
}

void jstrat_free(jstrat_common *const js)
{
  if (!js)
    return;
  if ((js->id & ~(integer)1) == 2)
    free((void*)(((jstrat_maneb2*)js)->tbl));
  (void)mset(js, 0u, sizeof(*js));
}

void jstrat_init_f(jstrat_common *const js, const integer *const id_, const integer *const n_, integer *const info_)
{
  assert(id_);
  assert(n_);
  assert(info_);
  *info_ = jstrat_init(js, *id_, *n_);
}

void jstrat_next_f(jstrat_common *const js, integer *const arr, integer *const info_)
{
  assert(info_);
  *info_ = jstrat_next(js, arr);
}

void jstrat_free_f(jstrat_common *const js)
{
  jstrat_free(js);
}

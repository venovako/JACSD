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
#include <cstdlib>
#include <cstring>
#else /* !__cplusplus */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#endif /* ?__cplusplus */

int jstrat_init(jstrat_common *const js, const int id, const int n)
{
  if (!js)
    return -1;
  if (id < 0)
    return -2;
  if (n < 2)
    return -3;

  if ((id & ~1) == 0) { /* row/col-cyclic */
    (void)memset(js, 0, sizeof(jstrat_rolcyc));
  }
  else if ((id & ~1) == 2) { /* Mantharam-Eberlein */
    const int *cur;

    if (n == 2)
      cur = &(ME0002[0][0][0]);
    else if (n == 4)
      cur = &(ME0004[0][0][0]);
    else if (n == 6)
      cur = &(ME0006[0][0][0]);
    else if (n == 8)
      cur = &(ME0008[0][0][0]);
    else if (n == 10)
      cur = &(ME0010[0][0][0]);
    else if (n == 12)
      cur = &(ME0012[0][0][0]);
    else if (n == 14)
      cur = &(ME0014[0][0][0]);
    else if (n == 16)
      cur = &(ME0016[0][0][0]);
    else if (n == 18)
      cur = &(ME0018[0][0][0]);
    else if (n == 20)
      cur = &(ME0020[0][0][0]);
    else if (n == 22)
      cur = &(ME0022[0][0][0]);
    else if (n == 24)
      cur = &(ME0024[0][0][0]);
    else if (n == 26)
      cur = &(ME0026[0][0][0]);
    else if (n == 28)
      cur = &(ME0028[0][0][0]);
    else if (n == 30)
      cur = &(ME0030[0][0][0]);
    else if (n == 32)
      cur = &(ME0032[0][0][0]);
    else if (n == 34)
      cur = &(ME0034[0][0][0]);
    else if (n == 36)
      cur = &(ME0036[0][0][0]);
    else if (n == 38)
      cur = &(ME0038[0][0][0]);
    else if (n == 40)
      cur = &(ME0040[0][0][0]);
    else if (n == 42)
      cur = &(ME0042[0][0][0]);
    else if (n == 44)
      cur = &(ME0044[0][0][0]);
    else if (n == 48)
      cur = &(ME0048[0][0][0]);
    else if (n == 52)
      cur = &(ME0052[0][0][0]);
    else if (n == 56)
      cur = &(ME0056[0][0][0]);
    else if (n == 60)
      cur = &(ME0060[0][0][0]);
    else if (n == 64)
      cur = &(ME0064[0][0][0]);
    else if (n == 128)
      cur = &(ME0128[0][0][0]);
    else if (n == 256)
      cur = &(ME0256[0][0][0]);
    else if (n == 512)
      cur = &(ME0512[0][0][0]);
    else if (n == 1024)
      cur = &(ME1024[0][0][0]);
    else
      return -3;

    jstrat_maneb2 *const me2 = (jstrat_maneb2*)memset(js, 0, sizeof(jstrat_maneb2));
    me2->nxt = me2->tbl = cur;
  }
  else
    return -2;

  js->id = id;
  js->n = n;
  return 0;
}

void
#ifdef _WIN32
JSTRAT_INIT
#else /* POSIX */
jstrat_init_
#endif /* ?_WIN32 */
(jstrat_common *const js, const int *const id_, const int *const n_, int *const info_)
{
  assert(id_);
  assert(n_);
  assert(info_);
  *info_ = jstrat_init(js, *id_, *n_);
}

int jstrat_next(jstrat_common *const js, int *const arr)
{
  if (!js)
    return -1;
  if (!arr)
    return -2;

  int info;
  if (js->id == 0) { /* row-cyclic */
    jstrat_rolcyc *const row = (jstrat_rolcyc*)js;
    int (*const pairs)[2] = (int (*)[2])arr;

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
  else if (js->id == 1) { /* column-cyclic */
    jstrat_rolcyc *const col = (jstrat_rolcyc*)js;
    int (*const pairs)[2] = (int (*)[2])arr;

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
  else if (js->id == 2) { /* Mantharam-Eberlein, no comm */
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    int (*const pairs)[2] = (int (*)[2])arr;

    const int n_2 = me2->n >> 1;
    for (int i = 0; i < n_2; ++i) {
      for (int k = 0; k < 2; ++k) {
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
  else if (js->id == 3) { /* Mantharam-Eberlein */
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    int (*const pairs)[2][2] = (int (*)[2][2])arr;

    const int *const cur = me2->nxt;
    const int n_2 = me2->n >> 1;
    for (int i = 0; i < n_2; ++i) {
      for (int k = 0; k < 2; ++k) {
        pairs[i][0][k] = *(me2->nxt);
        ++(me2->nxt);
      }
    }

    if (++(me2->stp) >= (me2->n - 1)) {
      me2->stp = 0;
      ++(me2->swp);
      me2->nxt = me2->tbl;
    }

    /* communication */
    for (int i = 0; i < me2->n; ++i) {
      for (int j = 0; j < me2->n; ++j) {
        /* cur[i] sent to nxt[j] */
        if (cur[i] == (me2->nxt)[j]) {
          const int k_1 = i & 1;
          const int k_2 = i >> 1;
          const int l_1 = j & 1;
          const int l_2 = j >> 1;
          const int snd = l_2 + 1;
          pairs[k_2][1][k_1] = (l_1 ? snd : -snd);
        }
      }
    }

    info = n_2;
  }
  else
    info = 0;

  return info;
}

void
#ifdef _WIN32
JSTRAT_NEXT
#else /* POSIX */
jstrat_next_
#endif /* ?_WIN32 */
(jstrat_common *const js, int *const arr, int *const info_)
{
  assert(info_);
  if ((*info_ = jstrat_next(js, arr)) > 0) {
    if (js->id < 3) {
      int (*const pairs)[2] = (int (*)[2])arr;
      for (int i = 0; i < *info_; ++i)
        for (int k = 0; k < 2; ++k)
          ++(pairs[i][k]);
    }
    else {
      int (*const pairs)[2][2] = (int (*)[2][2])arr;
      for (int i = 0; i < *info_; ++i)
        for (int k = 0; k < 2; ++k)
          ++(pairs[i][0][k]);
    }
  }
}

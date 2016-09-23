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

integer jstrat_init(jstrat_common *const js, const integer id, const integer n)
{
  if (!js)
    return (integer)-1;
  if (id < (integer)0)
    return (integer)-2;
  if (n < (integer)2)
    return (integer)-3;
  integer info /* = (integer)0 */;

  if ((id & ~(integer)1) == (integer)0) { /* row/col-cyclic */
    (void)memset(js, 0, sizeof(jstrat_rolcyc));
    if (n & (integer)1) /* n odd */
      info = n * ((n - (integer)1) >> 1);
    else /* n even */
      info = (n >> 1) * (n - (integer)1);
  }
  else if ((id & ~(integer)1) == (integer)2) { /* Mantharam-Eberlein */
    const integer *cur = (const integer*)NULL;

    if (n == (integer)2)
      cur = &(ME0002[0][0][0]);
    else if (n == (integer)4)
      cur = &(ME0004[0][0][0]);
    else if (n == (integer)6)
      cur = &(ME0006[0][0][0]);
    else if (n == (integer)8)
      cur = &(ME0008[0][0][0]);
    else if (n == (integer)10)
      cur = &(ME0010[0][0][0]);
    else if (n == (integer)12)
      cur = &(ME0012[0][0][0]);
    else if (n == (integer)14)
      cur = &(ME0014[0][0][0]);
    else if (n == (integer)16)
      cur = &(ME0016[0][0][0]);
    else if (n == (integer)18)
      cur = &(ME0018[0][0][0]);
    else if (n == (integer)20)
      cur = &(ME0020[0][0][0]);
    else if (n == (integer)22)
      cur = &(ME0022[0][0][0]);
    else if (n == (integer)24)
      cur = &(ME0024[0][0][0]);
    else if (n == (integer)26)
      cur = &(ME0026[0][0][0]);
    else if (n == (integer)28)
      cur = &(ME0028[0][0][0]);
    else if (n == (integer)30)
      cur = &(ME0030[0][0][0]);
    else if (n == (integer)32)
      cur = &(ME0032[0][0][0]);
    else if (n == (integer)34)
      cur = &(ME0034[0][0][0]);
    else if (n == (integer)36)
      cur = &(ME0036[0][0][0]);
    else if (n == (integer)38)
      cur = &(ME0038[0][0][0]);
    else if (n == (integer)40)
      cur = &(ME0040[0][0][0]);
    else if (n == (integer)42)
      cur = &(ME0042[0][0][0]);
    else if (n == (integer)44)
      cur = &(ME0044[0][0][0]);
    else if (n == (integer)48)
      cur = &(ME0048[0][0][0]);
    else if (n == (integer)52)
      cur = &(ME0052[0][0][0]);
    else if (n == (integer)56)
      cur = &(ME0056[0][0][0]);
    else if (n == (integer)60)
      cur = &(ME0060[0][0][0]);
    else if (n == (integer)64)
      cur = &(ME0064[0][0][0]);
    else if (n == (integer)128)
      cur = &(ME0128[0][0][0]);
    else if (n == (integer)256)
      cur = &(ME0256[0][0][0]);
    else if (n == (integer)512)
      cur = &(ME0512[0][0][0]);
    else if (n == (integer)1024)
      cur = &(ME1024[0][0][0]);
    else
      return (integer)-3;

    jstrat_maneb2 *const me2 = (jstrat_maneb2*)memset(js, 0, sizeof(jstrat_maneb2));
    me2->nxt = me2->tbl = cur;
    info = n - (integer)1;
  }
  else if (id == (integer)4) { /* modified modulus */
    if (n & (integer)1) /* n odd */
      return (integer)-3;
    (void)memset(js, 0, sizeof(jstrat_modmod));
    info = n;
  }
  else
    return (integer)-2;

  js->id = id;
  js->n = n;
  return info;
}

void jstrat_init_f(jstrat_common *const js, const integer *const id_, const integer *const n_, integer *const info_)
{
  assert(id_);
  assert(n_);
  assert(info_);
  *info_ = jstrat_init(js, *id_, *n_);
}

integer jstrat_next(jstrat_common *const js, integer *const arr)
{
  if (!js)
    return (integer)-1;
  if (!arr)
    return (integer)-2;
  integer info /* = (integer)0 */;

  if (js->id == (integer)0) { /* row-cyclic */
    jstrat_rolcyc *const row = (jstrat_rolcyc*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    if (++(row->q) >= (row->n)) {
      if (++(row->p) >= (row->n - (integer)1)) {
        row->p = (integer)0;
        row->q = (integer)1;
      }
      else
        row->q = row->p + (integer)1;
    }
    pairs[0][0] = row->p;
    pairs[0][1] = row->q;

    info = (integer)1;
  }
  else if (js->id == (integer)1) { /* column-cyclic */
    jstrat_rolcyc *const col = (jstrat_rolcyc*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    if (++(col->p) >= (col->q)) {
      if (++(col->q) >= (col->n)) {
        col->p = (integer)0;
        col->q = (integer)1;
      }
      else
        col->p = (integer)0;
    }

    pairs[0][0] = col->p;
    pairs[0][1] = col->q;

    info = (integer)1;
  }
  else if (js->id == (integer)2) { /* Mantharam-Eberlein, no comm */
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;

    const integer n_2 = me2->n >> 1;
    for (integer i = (integer)0; i < n_2; ++i) {
      for (integer k = (integer)0; k < (integer)2; ++k) {
        pairs[i][k] = *(me2->nxt);
        ++(me2->nxt);
      }
    }

    if (++(me2->stp) >= (me2->n - (integer)1)) {
      me2->stp = (integer)0;
      ++(me2->swp);
      me2->nxt = me2->tbl;
    }

    info = n_2;
  }
  else if (js->id == (integer)3) { /* Mantharam-Eberlein */
    jstrat_maneb2 *const me2 = (jstrat_maneb2*)js;
    integer (*const pairs)[2][2] = (integer (*)[2][2])arr;

    const integer *const cur = me2->nxt;
    const integer n_2 = me2->n >> 1;
    for (integer i = (integer)0; i < n_2; ++i) {
      for (integer k = (integer)0; k < (integer)2; ++k) {
        pairs[i][0][k] = *(me2->nxt);
        ++(me2->nxt);
      }
    }

    if (++(me2->stp) >= (me2->n - (integer)1)) {
      me2->stp = (integer)0;
      ++(me2->swp);
      me2->nxt = me2->tbl;
    }

    /* communication */
    for (integer i = (integer)0; i < me2->n; ++i) {
      for (integer j = (integer)0; j < me2->n; ++j) {
        /* cur[i] sent to nxt[j] */
        if (cur[i] == (me2->nxt)[j]) {
          const integer k_1 = i & (integer)1;
          const integer k_2 = i >> 1;
          const integer l_1 = j & (integer)1;
          const integer l_2 = j >> 1;
          const integer snd = l_2 + (integer)1;
          pairs[k_2][1][k_1] = (l_1 ? snd : -snd);
        }
      }
    }

    info = n_2;
  }
  else if (js->id == (integer)4) { /* modified modulus, no comm */
    jstrat_modmod *const mom = (jstrat_modmod*)js;
    integer (*const pairs)[2] = (integer (*)[2])arr;
    int (*const ij)[2][2] = (int (*)[2][2])pairs;

    const int _n = (int)(mom->n);
    const int _n_2 = _n >> 1;
    const int _n1 = _n - 1;

    if (!(mom->stp) && !(mom->swp)) { /* init */
      for (int r = 0; r < _n_2; ++r) {
        ij[r][0][1] = ij[r][0][0] = r;
        ij[r][1][1] = ij[r][1][0] = _n1 - r;
      }
    }
    else { /* step */
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
      mom->stp = (integer)0;
      ++(mom->swp);
    }
    info = (integer)-_n_2;
  }
  else
    info = (integer)0;

  return info;
}

void jstrat_next_f(jstrat_common *const js, integer *const arr, integer *const info_)
{
  assert(info_);
  if (*info_ = jstrat_next(js, arr)) {
    if (js->id != (integer)3) { /* no comm */
      integer (*const pairs)[2] = (integer (*)[2])arr;
      for (integer i = (integer)0; i < *info_; ++i)
        for (integer k = (integer)0; k < (integer)2; ++k)
          ++(pairs[i][k]);
    }
    else { /* comm */
      integer (*const pairs)[2][2] = (integer (*)[2][2])arr;
      for (integer i = (integer)0; i < *info_; ++i)
        for (integer k = (integer)0; k < (integer)2; ++k)
          ++(pairs[i][0][k]);
    }
  }
}

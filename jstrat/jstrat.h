#ifndef JSTRAT_H
#define JSTRAT_H

#ifdef EXTERN_C
#error EXTERN_C already defined
#else /* !EXTERN_C */
#ifdef __cplusplus
#define EXTERN_C extern "C"
#else /* !__cplusplus */
#define EXTERN_C extern
#endif /* ?__cplusplus */
#endif /* ?EXTERN_C */

#ifdef _WIN32
typedef long long integer;
#else /* POSIX */
typedef long integer;
#endif /* ?_WIN32 */

typedef struct {
  integer id, n;
  integer reserved[4];
} jstrat_common;

typedef struct {
  integer id, n, p, q;
  integer reserved[2];
} jstrat_rolcyc;

typedef struct {
  integer id, n, stp, swp;
  const integer *tbl; /* start of table */
  const integer *nxt; /* iterator */
} jstrat_maneb2;

typedef struct {
  integer id, n, stp, swp;
  integer reserved[2];
} jstrat_modmod;

/* MEMORY REQUIREMENTS:
 * struct jstrat_* = 6 * sizeof(integer) = 48 B
 * arr = #pivot pairs in a step * 2 components of a pair * (1 + (id > 1 && id & 1){communication}) * sizeof(integer)
 */

/* C interface */
EXTERN_C integer jstrat_init(jstrat_common *const js, const integer id, const integer n);
EXTERN_C integer jstrat_next(jstrat_common *const js, integer *const arr);

/* Fortran interface */
EXTERN_C void jstrat_init_f(jstrat_common *const js, const integer *const id_, const integer *const n_, integer *const info_);
EXTERN_C void jstrat_next_f(jstrat_common *const js, integer *const arr, integer *const info_);

#endif /* !JSTRAT_H */

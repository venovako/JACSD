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

#ifdef JSTRAT_ROWCYC
#error JSTRAT_ROWCYC already defined
#else /* !JSTRAT_ROWCYC */
#define JSTRAT_ROWCYC 0
#endif /* ?JSTRAT_ROWCYC */

#ifdef JSTRAT_COLCYC
#error JSTRAT_COLCYC already defined
#else /* !JSTRAT_COLCYC */
#define JSTRAT_COLCYC 1
#endif /* ?JSTRAT_COLCYC */

#ifdef JSTRAT_MANEB2
#error JSTRAT_MANEB2 already defined
#else /* !JSTRAT_MANEB2 */
#define JSTRAT_MANEB2 2
#endif /* ?JSTRAT_MANEB2 */

#ifdef JSTRAT_MECOMM
#error JSTRAT_MECOMM already defined
#else /* !JSTRAT_MECOMM */
#define JSTRAT_MECOMM 3
#endif /* ?JSTRAT_MECOMM */

typedef struct {
  int id, n;
} jstrat_common;

typedef struct {
  int id, n, p, q;
} jstrat_rolcyc;

typedef struct {
  int id, n, stp, swp;
  const int *tbl; /* start of table */
  const int *nxt; /* iterator */
} jstrat_maneb2;

/* C interface */
EXTERN_C int jstrat_init(jstrat_common *const js, const int id, const int n);
EXTERN_C int jstrat_next(jstrat_common *const js, int *const arr);

/* Fortran interface */
EXTERN_C void
#ifdef _WIN32
JSTRAT_INIT
#else /* POSIX */
jstrat_init_
#endif /* ?_WIN32 */
(jstrat_common *const js, const int *const id_, const int *const n_, int *const info_);
EXTERN_C void
#ifdef _WIN32
JSTRAT_NEXT
#else /* POSIX */
jstrat_next_
#endif /* ?_WIN32 */
(jstrat_common *const js, int *const arr, int *const info_);

#endif /* !JSTRAT_H */

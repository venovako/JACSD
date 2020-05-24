#ifndef DGESVJ_H
#define DGESVJ_H

// define to int, long, or long long
#ifndef integer
#define integer int
#endif /* !integer */

extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
void dgesvj_
(
 const char *const JOBA,
 const char *const JOBU,
 const char *const JOBV,
 const integer *const M,
 const integer *const N,
 double *const A,
 const integer *const LDA,
 double *const SVA,
 const integer *const MV,
 double *const V,
 const integer *const LDV,
 double *const WORK,
 const integer *const LWORK,
 integer *const INFO
);

#endif /* !DGESVJ_H */

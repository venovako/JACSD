#ifndef AVX2_FMA_DJACV_H
#define AVX2_FMA_DJACV_H

extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
unsigned long avx2_fma_djacv0_execute(const unsigned m, const unsigned n, double *const restrict G, double *const restrict V, const double tol, const int swp, uintptr_t *const restrict Iwork, int *const restrict info);

#endif /* !AVX2_FMA_DJACV_H */

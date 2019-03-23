#ifndef VN_ALLOC_H
#define VN_ALLOC_H

#ifndef VN_LIB_H
#error vn_alloc.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef __APPLE__
#include <alloca.h>
#else /* !__APPLE__ */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif /* !_GNU_SOURCE */
#include <alloca.h>
#include <malloc.h>
#endif /* ?__APPLE__ */
#include <stdlib.h>
/*
  act:
   0 - compute ldA
   1 - allocate in RAM
   2 - allocate in HBM (Only on the KNLs!)
  -1 - also zero-out the allocation in RAM
  -2 - also zero-out the allocation in HBM
*/

VN_EXTERN_C void *vn_alloc1(const vn_integer m, const size_t szT, vn_integer *const ldA, const vn_integer act);
VN_EXTERN_C void *vn_alloc2(const vn_integer m, const vn_integer n, const size_t szT, vn_integer *const ldA, const vn_integer act);
VN_EXTERN_C void vn_free(const void *const ptr);
VN_EXTERN_C void vn_freep(const void **const pptr);

#ifndef VN_ALLOC1
#define VN_ALLOC1(T, m, ldA, act) (T *)vn_alloc1((m), sizeof(T), (ldA), (act))
#else /* VN_ALLOC1 */
#error VN_ALLOC1 already defined
#endif /* !VN_ALLOC1 */

#ifndef VN_ALLOC2
#define VN_ALLOC2(T, m, n, ldA, act) (T *)vn_alloc2((m), (n), sizeof(T), (ldA), (act))
#else /* VN_ALLOC2 */
#error VN_ALLOC2 already defined
#endif /* !VN_ALLOC2 */

#endif /* !VN_ALLOC_H */

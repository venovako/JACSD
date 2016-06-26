#ifndef VN_ERROR_H
#define VN_ERROR_H

#ifndef VN_LIB_H
#error vn_error.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifdef __cplusplus
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#else /* C99 */
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif /* __cplusplus */

#ifndef VN_STOP
#define VN_STOP(msg) {                                                  \
    (void)fprintf(stderr, "%s(%d): %s\n", __FILE__, __LINE__, (msg));   \
    exit(EXIT_FAILURE);                                                 \
  }
#else /* VN_STOP */
#error VN_STOP not definable externally
#endif /* !VN_STOP */

#ifndef VN_SYSI_CALL
#define VN_SYSI_CALL(call) {                                            \
    if (0 != (int)(call)) {                                             \
      (void)fprintf(stderr, "%s(%d): %s",                               \
                    __FILE__, __LINE__, strerror(errno));               \
      exit(EXIT_FAILURE);                                               \
    }                                                                   \
  }
#else /* VN_SYSI_CALL */
#error VN_SYSI_CALL already defined
#endif /* !VN_SYSI_CALL */

#ifndef VN_SYSP_CALL
#define VN_SYSP_CALL(call) {                                            \
    if (NULL == (const void*)(call)) {                                  \
      (void)fprintf(stderr, "%s(%d): %s",                               \
                    __FILE__, __LINE__, strerror(errno));               \
      exit(EXIT_FAILURE);                                               \
    }                                                                   \
  }
#else /* VN_SYSP_CALL */
#error VN_SYSP_CALL already defined
#endif /* !VN_SYSP_CALL */

#endif /* !VN_ERROR_H */

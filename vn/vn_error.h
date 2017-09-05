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

#include <execinfo.h>
#include <pthread.h>
#include <unistd.h>

#ifndef VN_BTRACE_BUFSIZ
#define VN_BTRACE_BUFSIZ 128
#else /* VN_BTRACE_BUFSIZ */
#error VN_BTRACE_BUFSIZ already defined
#endif /* !VN_BTRACE_BUFSIZ */

#ifndef VN_BTRACE
#define VN_BTRACE {                                      \
    void* buffer[VN_BTRACE_BUFSIZ];                      \
    const int bsz = backtrace(buffer, VN_BTRACE_BUFSIZ); \
    if (bsz > 0) {                                       \
      backtrace_symbols_fd(buffer, bsz, STDERR_FILENO);  \
      (void)fsync(STDERR_FILENO);                        \
    }                                                    \
  }
#else /* VN_BTRACE */
#error VN_BTRACE already defined
#endif /* !VN_BTRACE */

#ifndef VN_STOP
#define VN_STOP(msg) {                                   \
    if (msg)                                             \
      (void)fprintf(stderr, "%s(%d) in thread %p: %s\n", \
                    __FILE__, __LINE__,                  \
                    (const void*)pthread_self(), (msg)); \
    else                                                 \
      (void)fprintf(stderr, "%s(%d) in thread %p:\n",    \
                    __FILE__, __LINE__,                  \
                    (const void*)pthread_self());        \
    (void)fflush(stderr);                                \
    VN_BTRACE;                                           \
    exit(EXIT_FAILURE);                                  \
  }
#else /* VN_STOP */
#error VN_STOP already defined
#endif /* !VN_STOP */

#ifndef VN_SYSI_CALL
#define VN_SYSI_CALL(call) {                             \
    if (0 != (int)(call))                                \
      VN_STOP(strerror(errno));                          \
  }
#else /* VN_SYSI_CALL */
#error VN_SYSI_CALL already defined
#endif /* !VN_SYSI_CALL */

#ifndef VN_SYSP_CALL
#define VN_SYSP_CALL(call) {                             \
    if (NULL == (const void*)(call))                     \
      VN_STOP(strerror(errno));                          \
  }
#else /* VN_SYSP_CALL */
#error VN_SYSP_CALL already defined
#endif /* !VN_SYSP_CALL */

#endif /* !VN_ERROR_H */

#ifndef SSE_ENV_H
#define SSE_ENV_H
#include <xmmintrin.h>

#ifndef DEFAULT_SSE_MODE
#define DEFAULT_SSE_MODE 0x1F80u
#endif /* !DEFAULT_SSE_MODE */

static inline unsigned set_sse_mode(const unsigned m)
{
  const unsigned old = _mm_getcsr();
  if (m != old)
    _mm_setcsr(m);
  return old;
}
#endif /* !SSE_ENV_H */

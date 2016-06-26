#ifndef VN_LIB_H
#define VN_LIB_H

#include "vn_attrs.h"
#include "vn_assert.h"
#include "vn_types.h"
#include "vn_error.h"
#include "vn_simd.h"
#include "vn_align.h"
#include "vn_alloc.h"
#include "vn_timer.h"

#include "vn_blas.h"
#include "vn_lapack.h"

#include "vn_bmp.h"
#include "vn_mtxvis.h"

#ifdef __cplusplus
#include <cctype>
#include <cfenv>
#include <csignal>
#else /* C99 */
#include <ctype.h>
#include <fenv.h>
#include <signal.h>
#endif /* __cplusplus */

#endif /* !VN_LIB_H */

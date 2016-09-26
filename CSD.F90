MODULE CSD

#include "USE_MODULES.F90"

  IMPLICIT NONE

#include "KIND_PARAMS.F90"
#include "VEC_PARAMS.F90"
#include "CONSTANTS.F90"

#include "INTERFACES.F90"

CONTAINS

#include "IFACES_IMPL.F90"

! Threading support.
#include "GET_NTHR.F90"
#include "BLAS.F90"

! I/O support.
#include "GET_IOUNIT.F90"
#include "BIN_IO.F90"

! Timing support.
#include "TIMER.F90"

#ifdef USE_MKL
#include "mkl_direct_call.fi"
#endif

#include "JAC0.F90"
#include "JAC1.F90"
#include "JAC2.F90"

END MODULE CSD

MODULE FTNUTILS

#include "USE_MODULES.F90"

  IMPLICIT NONE

#include "KIND_PARAMS.F90"
#include "VEC_PARAMS.F90"
#include "CONSTANTS.F90"
#include "INTERFACES.F90"
#include "BIN_IO_IFACES.F90"
#ifdef USE_MKL
#ifndef USE_INTEL
#include "mkl_service.fi"
#endif
#endif

CONTAINS

#include "IFACES_IMPL.F90"

! I/O support.
#include "GET_IOUNIT.F90"
#include "BIN_IO.F90"

! Threading support.
#include "GET_NTHR.F90"
#include "BLAS.F90"

! Timing support.
#include "TIMER.F90"

#ifdef MKL_DIRECT_CALL
#include "mkl_direct_call.fi"
#endif

END MODULE FTNUTILS

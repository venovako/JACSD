MODULE FTNUTILS

#include "USE_MODULES.F90"

  IMPLICIT NONE

#include "KIND_PARAMS.F90"
#include "VEC_PARAMS.F90"
#include "CONSTANTS.F90"
#include "INTERFACES.F90"
#include "BIN_IO_IFACES.F90"

CONTAINS

#include "IFACES_IMPL.F90"

! I/O support.
#include "GET_IOUNIT.F90"
#include "BIN_IO.F90"

! Threading support.
#include "GET_NTHR.F90"

! Timing support.
#include "TIMER.F90"

END MODULE FTNUTILS

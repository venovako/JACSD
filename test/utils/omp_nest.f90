PROGRAM OMP_NEST
  USE OMP_LIB
  IMPLICIT NONE
  WRITE (*,*) OMP_GET_MAX_ACTIVE_LEVELS()
END PROGRAM OMP_NEST

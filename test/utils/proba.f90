PROGRAM PROBA
  IMPLICIT NONE
  INCLUDE 'nrmlz.fi'
  DOUBLE COMPLEX :: Z
  Z = DCMPLX(HUGE(0.0D0),-HUGE(0.0D0))
  Z = ZNRMLZ(Z)
  PRINT *, Z
END PROGRAM PROBA

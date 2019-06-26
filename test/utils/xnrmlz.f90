PROGRAM XNRMLZ
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  INCLUDE 'nrmlz.fi'
  COMPLEX(c_double) :: Z
  Z = CMPLX(HUGE(0.0_c_double), -HUGE(0.0_c_double), c_double)
  Z = ZNRMLZ(Z)
  WRITE (*,*) Z
END PROGRAM XNRMLZ

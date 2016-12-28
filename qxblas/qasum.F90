!> \brief \b QASUM
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QASUM(N,SX,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       REAL(WP) SX(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    QASUM takes the sum of the absolute values.
!>    uses unrolled loops for increment equal to one.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup single_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
FUNCTION QASUM(N,SX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: SX(*)
!     ..
  REAL(WP) :: QASUM
!
!  =====================================================================
!
!     .. Local Scalars ..
  REAL(WP) :: STEMP
  INTEGER :: I,NINCX
!     ..
  QASUM = 0.0e0_WP
  STEMP = 0.0e0_WP
  IF ((N .LE. 0) .OR. (INCX .LE. 0)) RETURN
  IF (INCX .EQ. 1) THEN
!
!        code for increment equal to 1
!
     DO I = 1,N
        STEMP = STEMP + ABS(SX(I))
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     NINCX = N*INCX
     DO I = 1,NINCX,INCX
        STEMP = STEMP + ABS(SX(I))
     END DO
  END IF
  QASUM = STEMP
END FUNCTION QASUM

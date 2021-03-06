!> \brief \b QXASUM
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QXASUM(N,CX,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) CX(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    QXASUM takes the sum of the (|Re(.)| + |Im(.)|)'s of a complex vector and
!>    returns a single precision result.
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
!> \date November 2015
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
FUNCTION QXASUM(N,CX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.6.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2015
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(IN) :: CX(*)
!     ..
!
  REAL(WP) :: QXASUM
!
!  =====================================================================
!
!     .. Local Scalars ..
  REAL(WP) :: STEMP
  INTEGER :: I,NINCX
!     ..
  QXASUM = 0.0E0_WP
  STEMP = 0.0E0_WP
  IF ((N .LE. 0) .OR. (INCX .LE. 0)) RETURN
  IF (INCX .EQ. 1) THEN
!
!        code for increment equal to 1
!
     DO I = 1,N
        STEMP = STEMP + ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     NINCX = N*INCX
     DO I = 1,NINCX,INCX
        STEMP = STEMP + ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
     END DO
  END IF
  QXASUM = STEMP
!
END FUNCTION QXASUM

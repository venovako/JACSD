!> \brief \b QDOT
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QDOT(N,SX,INCX,SY,INCY)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       REAL(WP) SX(*),SY(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    QDOT forms the dot product of two vectors.
!>    uses unrolled loops for increments equal to one.
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
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      FUNCTION QDOT(N,SX,INCX,SY,INCY)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,INCY,N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: SX(*),SY(*)
!     ..
  REAL(WP) :: QDOT
!
!  =====================================================================
!
!     .. Local Scalars ..
  REAL(WP) :: STEMP
  INTEGER :: I,IX,IY
!     ..
  STEMP = 0.0e0_WP
  QDOT = 0.0e0_WP
  IF (N .LE. 0) RETURN
  IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
!
!        code for both increments equal to 1
!
     DO I = 1,N
        STEMP = STEMP + SX(I)*SY(I)
     END DO
  ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
     IX = 1
     IY = 1
     IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
     IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
     DO I = 1,N
        STEMP = STEMP + SX(IX)*SY(IY)
        IX = IX + INCX
        IY = IY + INCY
     END DO
  END IF
  QDOT = STEMP
END FUNCTION QDOT

!> \brief \b QAXPY
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QAXPY(N,SA,SX,INCX,SY,INCY)
! 
!       .. Scalar Arguments ..
!       REAL(WP) SA
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
!>    QAXPY constant times a vector plus a vector.
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
SUBROUTINE QAXPY(N,SA,SX,INCX,SY,INCY)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: SA
  INTEGER, INTENT(IN) :: INCX,INCY,N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: SX(*)
  REAL(WP), INTENT(INOUT) :: SY(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I,IX,IY
!     ..
  IF (N .LE. 0) RETURN
  IF (SA .EQ. 0.0_WP) RETURN
  IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
!
!        code for both increments equal to 1
!
     DO I = 1,N
        SY(I) = SY(I) + SA*SX(I)
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
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
     END DO
  END IF
END SUBROUTINE QAXPY

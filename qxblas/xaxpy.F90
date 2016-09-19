!> \brief \b XAXPY
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XAXPY(N,CA,CX,INCX,CY,INCY)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) CA
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) CX(*),CY(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    XAXPY constant times a vector plus a vector.
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
!> \ingroup complex_blas_level1
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
SUBROUTINE XAXPY(N,CA,CX,INCX,CY,INCY)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  COMPLEX(WP), INTENT(IN) :: CA
  INTEGER, INTENT(IN) :: INCX,INCY,N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(IN) :: CX(*)
  COMPLEX(WP), INTENT(INOUT) :: CY(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I,IX,IY
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QXABS1
!     ..
  IF (N .LE. 0) RETURN
  IF (QXABS1(CA) .EQ. 0.0E+0_WP) RETURN
  IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
!
!        code for both increments equal to 1
!
     DO I = 1,N
        CY(I) = CY(I) + CA*CX(I)
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
        CY(IY) = CY(IY) + CA*CX(IX)
        IX = IX + INCX
        IY = IY + INCY
     END DO
  END IF
!
END SUBROUTINE XAXPY

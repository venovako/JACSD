!> \brief \b XSWAP
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XSWAP(N,CX,INCX,CY,INCY)
! 
!       .. Scalar Arguments ..
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
!>   XSWAP interchanges two vectors.
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
SUBROUTINE XSWAP(N,CX,INCX,CY,INCY)
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
  INTEGER, INTENT(IN) :: INCX,INCY,N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(INOUT) :: CX(*),CY(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  COMPLEX(WP) :: CTEMP
  INTEGER :: I,IX,IY
!     ..
  IF (N .LE. 0) RETURN
  IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
!
!       code for both increments equal to 1
     DO I = 1,N
        CTEMP = CX(I)
        CX(I) = CY(I)
        CY(I) = CTEMP
     END DO
  ELSE
!
!       code for unequal increments or equal increments not equal to 1
!
     IX = 1
     IY = 1
     IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
     IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
     DO I = 1,N
        CTEMP = CX(IX)
        CX(IX) = CY(IY)
        CY(IY) = CTEMP
        IX = IX + INCX
        IY = IY + INCY
     END DO
  END IF
END SUBROUTINE XSWAP

!> \brief \b IXAMAX
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION IXAMAX(N,CX,INCX)
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
!>    IXAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
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
!> \ingroup aux_blas
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
INTEGER FUNCTION IXAMAX(N,CX,INCX)
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
!  =====================================================================
!
!     .. Local Scalars ..
  REAL(WP) :: SMAX
  INTEGER :: I,IX
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QXABS1
!     ..
  IXAMAX = 0
  IF ((N .LT. 1) .OR. (INCX .LE. 0)) RETURN
  IXAMAX = 1
  IF (N .EQ. 1) RETURN
  IF (INCX .EQ. 1) THEN
!
!        code for increment equal to 1
!
     SMAX = QXABS1(CX(1))
     DO I = 2,N
        IF (QXABS1(CX(I)) .GT. SMAX) THEN
           IXAMAX = I
           SMAX = QXABS1(CX(I))
        END IF
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     IX = 1
     SMAX = QXABS1(CX(1))
     IX = IX + INCX
     DO I = 2,N
        IF (QXABS1(CX(IX)) .GT. SMAX) THEN
           IXAMAX = I
           SMAX = QXABS1(CX(IX))
        END IF
        IX = IX + INCX
     END DO
  END IF
!
END FUNCTION IXAMAX

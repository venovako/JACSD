!> \brief \b IQAMAX
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION IQAMAX(N,SX,INCX)
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
!>    IQAMAX finds the index of the first element having maximum absolute value.
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
INTEGER FUNCTION IQAMAX(N,SX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.6.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2015
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: SX(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  REAL(WP) :: SMAX
  INTEGER :: I,IX
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS
!     ..
  IQAMAX = 0
  IF ((N .LT. 1) .OR. (INCX .LE. 0)) RETURN
  IQAMAX = 1
  IF (N .EQ. 1) RETURN
  IF (INCX .EQ. 1) THEN
!
!        code for increment equal to 1
!
     SMAX = ABS(SX(1))
     DO I = 2,N
        IF (ABS(SX(I)) .GT. SMAX) THEN
           IQAMAX = I
           SMAX = ABS(SX(I))
        END IF
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     IX = 1
     SMAX = ABS(SX(1))
     IX = IX + INCX
     DO I = 2,N
        IF (ABS(SX(IX)) .GT. SMAX) THEN
           IQAMAX = I
           SMAX = ABS(SX(IX))
        END IF
        IX = IX + INCX
     END DO
  END IF
!
END FUNCTION IQAMAX

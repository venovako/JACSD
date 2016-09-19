!> \brief \b QROTG
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QROTG(SA,SB,C,S)
! 
!       .. Scalar Arguments ..
!       REAL(WP) C,S,SA,SB
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    QROTG construct givens plane rotation.
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
!
!> \ingroup single_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE QROTG(SA, SB, C, S)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  REAL(WP), INTENT(INOUT) ::  SA, SB
  REAL(WP), INTENT(OUT) ::  C, S
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP, ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: R, ROE, SCAL, Z
!     ..
!     .. Intrinsic Functions ..
#ifndef USE_IBM
  REAL(WP), INTRINSIC :: ABS, HYPOT, SIGN
#endif
!     ..
  ROE = SB
  IF (ABS(SA) .GT. ABS(SB)) ROE = SA
  SCAL = ABS(SA) + ABS(SB)
  IF (SCAL .EQ. ZERO) THEN
     C = ONE
     S = ZERO
     R = ZERO
     Z = ZERO
  ELSE
     R = SCAL * HYPOT((SA/SCAL),(SB/SCAL))
     R = SIGN(ONE,ROE) * R
     C = SA / R
     S = SB / R
     Z = ONE
     IF (ABS(SA) .GT. ABS(SB)) Z = S
     IF ((ABS(SB) .GE. ABS(SA)) .AND. (C .NE. ZERO)) Z = ONE/C
  END IF
  SA = R
  SB = Z

END SUBROUTINE QROTG

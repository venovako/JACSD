!> \brief \b XROTG
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XROTG(CA,CB,C,S)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) CA,CB,S
!       REAL(WP) C
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XROTG determines a complex Givens rotation.
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
!  =====================================================================
SUBROUTINE XROTG(CA,CB,C,S)
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
  COMPLEX(WP), INTENT(INOUT) :: CA
  COMPLEX(WP), INTENT(IN) :: CB
  COMPLEX(WP), INTENT(OUT) :: S
  REAL(WP), INTENT(OUT) :: C
!     ..
!
!  =====================================================================
!
  REAL(WP), PARAMETER :: RZERO = 0.0E+0_WP
  REAL(WP), PARAMETER :: RONE = 1.0E+0_WP
!     .. Local Scalars ..
  COMPLEX(WP) :: ALPHA
  REAL(WP) :: NORM, ACA
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS,HYPOT
  COMPLEX(WP), INTRINSIC :: CONJG
!     ..
  ACA = ABS(CA)
  IF (ACA .EQ. RZERO) THEN
     C = RZERO
     S = (RONE,RZERO)
     CA = CB
  ELSE
     NORM = HYPOT(ACA, ABS(CB))
     ALPHA = CA / ACA
     C = ACA / NORM
     S = ALPHA * CONJG(CB)/NORM
     CA = ALPHA * NORM
  END IF
END SUBROUTINE XROTG

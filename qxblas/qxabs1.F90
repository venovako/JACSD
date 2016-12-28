!> \brief \b QXABS1
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QXABS1(Z)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) Z
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QXABS1 computes |Re(.)| + |Im(.)| of a complex number
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
!  =====================================================================
FUNCTION QXABS1(Z)
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
  COMPLEX(WP), INTENT(IN) :: Z
!     ..
!
  REAL(WP) :: QXABS1
!
!  =====================================================================
!
!     ..
  QXABS1 = ABS(REAL(Z)) + ABS(AIMAG(Z))
!
END FUNCTION QXABS1

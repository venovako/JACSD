!> \brief \b QLAPY2 returns sqrt(x2+y2).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLAPY2 + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlapy2.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlapy2.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlapy2.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QLAPY2( X, Y )
! 
!       .. Scalar Arguments ..
!       REAL(WP)           X, Y
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!> overflow.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] X
!> \verbatim
!>          X is REAL(WP)
!> \endverbatim
!>
!> \param[in] Y
!> \verbatim
!>          Y is REAL(WP)
!>          X and Y specify the values x and y.
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
!> \date September 2012
!
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
FUNCTION QLAPY2(X, Y)
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: X, Y
!     ..
!
  REAL(WP) :: QLAPY2
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: W, XABS, YABS, Z
!     ..
!     .. Executable Statements ..
!
  XABS = ABS(X)
  YABS = ABS(Y)
  W = MAX(XABS, YABS)
  Z = MIN(XABS, YABS)
  IF (Z .EQ. ZERO) THEN
     QLAPY2 = W
  ELSE
     QLAPY2 = W*SQRT(ONE + (Z/W)**2)
  END IF
!
!     End of QLAPY2
!
END FUNCTION QLAPY2

!> \brief \b QLAMCH
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!      REAL(WP)           FUNCTION QLAMCH( CMACH )
!
!     .. Scalar Arguments ..
!      CHARACTER          CMACH
!     ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLAMCH determines single precision machine parameters.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] CMACH
!> \verbatim
!>          Specifies the value to be returned by QLAMCH:
!>          = 'E' or 'e',   QLAMCH := eps
!>          = 'S' or 's ,   QLAMCH := sfmin
!>          = 'B' or 'b',   QLAMCH := base
!>          = 'P' or 'p',   QLAMCH := eps*base
!>          = 'N' or 'n',   QLAMCH := t
!>          = 'R' or 'r',   QLAMCH := rnd
!>          = 'M' or 'm',   QLAMCH := emin
!>          = 'U' or 'u',   QLAMCH := rmin
!>          = 'L' or 'l',   QLAMCH := emax
!>          = 'O' or 'o',   QLAMCH := rmax
!>          where
!>          eps   = relative machine precision
!>          sfmin = safe minimum, such that 1/sfmin does not overflow
!>          base  = base of the machine
!>          prec  = eps*base
!>          t     = number of (base) digits in the mantissa
!>          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
!>          emin  = minimum exponent before (gradual) underflow
!>          rmin  = underflow threshold - base**(emin-1)
!>          emax  = largest exponent before overflow
!>          rmax  = overflow threshold  - (base**emax)*(1-eps)
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
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
FUNCTION QLAMCH(CMACH)
!
!  -- LAPACK auxiliary routine (version 3.4.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: CMACH
!     ..
  REAL(WP) :: QLAMCH
!
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP, ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: RND, EPS, SFMIN, SMALL, RMACH
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. Executable Statements ..
!
!
!     Assume rounding, not chopping. Always.
!
  RND = ONE
!
  IF (ONE .EQ. RND) THEN
     EPS = EPSILON(ZERO) * 0.5_WP
  ELSE
     EPS = EPSILON(ZERO)
  END IF
!
  IF (LSAME(CMACH, 'E')) THEN
     RMACH = EPS
  ELSE IF (LSAME(CMACH, 'S')) THEN
     SFMIN = TINY(ZERO)
     SMALL = ONE / HUGE(ZERO)
     IF (SMALL .GE. SFMIN) THEN
!
!           Use SMALL plus a bit, to avoid the possibility of rounding
!           causing overflow when computing  1/sfmin.
!
        SFMIN = SMALL*(ONE+EPS)
     END IF
     RMACH = SFMIN
  ELSE IF (LSAME(CMACH, 'B')) THEN
     RMACH = RADIX(ZERO)
  ELSE IF (LSAME(CMACH, 'P')) THEN
     RMACH = EPS * RADIX(ZERO)
  ELSE IF (LSAME(CMACH, 'N')) THEN
     RMACH = DIGITS(ZERO)
  ELSE IF (LSAME(CMACH, 'R')) THEN
     RMACH = RND
  ELSE IF (LSAME(CMACH, 'M')) THEN
     RMACH = MINEXPONENT(ZERO)
  ELSE IF (LSAME(CMACH, 'U')) THEN
     RMACH = TINY(ZERO)
  ELSE IF (LSAME(CMACH, 'L')) THEN
     RMACH = MAXEXPONENT(ZERO)
  ELSE IF (LSAME(CMACH, 'O')) THEN
     RMACH = HUGE(ZERO)
  ELSE
     RMACH = ZERO
  END IF
!
  QLAMCH = RMACH
  RETURN
!
!     End of QLAMCH
!
END FUNCTION QLAMCH

!> \brief \b QLARND
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP)           FUNCTION QLARND( IDIST, ISEED )
! 
!       .. Scalar Arguments ..
!       INTEGER            IDIST
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLARND returns a random real number from a uniform or normal
!> distribution.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] IDIST
!> \verbatim
!>          IDIST is INTEGER
!>          Specifies the distribution of the random numbers:
!>          = 1:  uniform (0,1)
!>          = 2:  uniform (-1,1)
!>          = 3:  normal (0,1)
!> \endverbatim
!>
!> \param[in,out] ISEED
!> \verbatim
!>          ISEED is INTEGER array, dimension (4)
!>          On entry, the seed of the random number generator; the array
!>          elements must be between 0 and 4095, and ISEED(4) must be
!>          odd.
!>          On exit, the seed is updated.
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
!> \ingroup real_matgen
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  This routine calls the auxiliary routine SLARAN to generate a random
!>  real number from a uniform (0,1) distribution. The Box-Muller method
!>  is used to transform numbers from a uniform to a normal distribution.
!> \endverbatim
!>
!  =====================================================================
FUNCTION QLARND(IDIST, ISEED)
!
!  -- LAPACK auxiliary routine (version 3.4.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: IDIST
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
!     ..
!
  REAL(WP) :: QLARND
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP, TWO = 2.0E+0_WP
  REAL(WP), PARAMETER :: TWOPI = 6.2831853071795864769252867663E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: T1, T2
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QLARAN
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: COS, LOG, SQRT
!     ..
!     .. Executable Statements ..
!
!     Generate a real random number from a uniform (0,1) distribution
!
  T1 = QLARAN(ISEED)
!
  IF (IDIST .EQ. 1) THEN
!
!        uniform (0,1)
!
     QLARND = T1
  ELSE IF (IDIST .EQ. 2) THEN
!
!        uniform (-1,1)
!
     QLARND = TWO*T1 - ONE
  ELSE IF (IDIST .EQ. 3) THEN
!
!        normal (0,1)
!
     T2 = QLARAN(ISEED)
     QLARND = SQRT(-TWO*LOG(T1)) * COS(TWOPI*T2)
  ELSE
     QLARND = ONE
  END IF
!
!     End of QLARND
!
END FUNCTION QLARND

!> \brief \b XLARNV returns a vector of random numbers from a uniform or normal distribution.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE XLARNV( IDIST, ISEED, N, X )
!
!       .. Scalar Arguments ..
!       INTEGER            IDIST, N
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       COMPLEX(WP)        X( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLARNV returns a vector of n random complex numbers from a uniform or
!> normal distribution.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] IDIST
!> \verbatim
!>          IDIST is INTEGER
!>          Specifies the distribution of the random numbers:
!>          = 1:  real and imaginary parts each uniform (0,1)
!>          = 2:  real and imaginary parts each uniform (-1,1)
!>          = 3:  real and imaginary parts each normal (0,1)
!>          = 4:  uniformly distributed on the disc abs(z) < 1
!>          = 5:  uniformly distributed on the circle abs(z) = 1
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
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of random numbers to be generated.
!> \endverbatim
!>
!> \param[out] X
!> \verbatim
!>          X is COMPLEX(WP) array, dimension (N)
!>          The generated random numbers.
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
!> \date December 2016
!
!> \ingroup complex16OTHERauxiliary
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  This routine calls the auxiliary routine QLARUV to generate random
!>  real numbers from a uniform (0,1) distribution, in batches of up to
!>  128 using vectorisable code. The Box-Muller method is used to
!>  transform numbers from a uniform to a normal distribution.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE XLARNV(IDIST, ISEED, N, X)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: IDIST, N
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
  COMPLEX(WP), INTENT(OUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP, ONE = 1.0E+0_WP, TWO = 2.0E+0_WP
  INTEGER, PARAMETER :: LV = 128
  REAL(WP), PARAMETER :: TWOPI = 6.2831853071795864769252867663E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, IL, IV
!     ..
!     .. Local Arrays ..
  REAL(WP) :: U(LV)
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QLARUV
!     ..
!     .. Executable Statements ..
!
  DO IV = 1, N, LV/2
     IL = MIN(LV/2, N-IV+1)
!
!        Call QLARUV to generate 2*IL real numbers from a uniform (0,1)
!        distribution (2*IL <= LV)
!
     CALL QLARUV( ISEED, 2*IL, U )
!
     IF (IDIST .EQ. 1) THEN
!
!           Copy generated numbers
!
        DO I = 1, IL
           X(IV+I-1) = CMPLX(U(2*I-1), U(2*I), WP)
        END DO
     ELSE IF (IDIST .EQ. 2) THEN
!
!           Convert generated numbers to uniform (-1,1) distribution
!
        DO I = 1, IL
           X(IV+I-1) = CMPLX(TWO*U(2*I-1)-ONE, TWO*U(2*I)-ONE, WP)
        END DO
     ELSE IF (IDIST .EQ. 3) THEN
!
!           Convert generated numbers to normal (0,1) distribution
!
        DO I = 1, IL
           X(IV+I-1) = SQRT(-TWO*LOG(U(2*I-1))) * EXP(CMPLX(ZERO, TWOPI*U(2*I), WP))
        END DO
     ELSE IF (IDIST .EQ. 4) THEN
!
!           Convert generated numbers to complex numbers uniformly
!           distributed on the unit disk
!
        DO I = 1, IL
           X(IV+I-1) = SQRT(U(2*I-1)) * EXP(CMPLX(ZERO, TWOPI*U(2*I), WP))
        END DO
     ELSE IF (IDIST .EQ. 5) THEN
!
!           Convert generated numbers to complex numbers uniformly
!           distributed on the unit circle
!
        DO I = 1, IL
           X(IV+I-1) = EXP(CMPLX(ZERO, TWOPI*U(2*I), WP))
        END DO
     END IF
  END DO
!
!     End of XLARNV
!
END SUBROUTINE XLARNV

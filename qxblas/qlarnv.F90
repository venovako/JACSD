!> \brief \b QLARNV returns a vector of random numbers from a uniform or normal distribution.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLARNV( IDIST, ISEED, N, X )
!
!       .. Scalar Arguments ..
!       INTEGER            IDIST, N
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       REAL(WP)           X( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLARNV returns a vector of n random real numbers from a uniform or
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
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of random numbers to be generated.
!> \endverbatim
!>
!> \param[out] X
!> \verbatim
!>          X is REAL(WP) array, dimension (N)
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
!> \ingroup OTHERauxiliary
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
SUBROUTINE QLARNV(IDIST, ISEED, N, X)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: IDIST, N
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
  REAL(WP), INTENT(OUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP, TWO = 2.0E+0_WP
  INTEGER, PARAMETER :: LV = 128
  REAL(WP), PARAMETER :: TWOPI = 6.2831853071795864769252867663E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, IL, IL2, IV
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
     IF (IDIST .EQ. 3) THEN
        IL2 = 2*IL
     ELSE
        IL2 = IL
     END IF
!
!        Call QLARUV to generate IL2 numbers from a uniform (0,1)
!        distribution (IL2 <= LV)
!
     CALL QLARUV(ISEED, IL2, U)
!
     IF (IDIST .EQ. 1) THEN
!
!           Copy generated numbers
!
        DO I = 1, IL
           X(IV+I-1) = U(I)
        END DO
     ELSE IF (IDIST .EQ. 2) THEN
!
!           Convert generated numbers to uniform (-1,1) distribution
!
        DO I = 1, IL
           X(IV+I-1) = TWO*U(I) - ONE
        END DO
     ELSE IF (IDIST .EQ. 3) THEN
!
!           Convert generated numbers to normal (0,1) distribution
!
        DO I = 1, IL
           X(IV+I-1) = SQRT(-TWO*LOG(U(2*I-1))) * COS(TWOPI*U(2*I))
        END DO
     END IF
  END DO
!
!     End of QLARNV
!
END SUBROUTINE QLARNV

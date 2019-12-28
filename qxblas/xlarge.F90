!> \brief \b XLARGE
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE XLARGE( N, A, LDA, ISEED, WORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, LDA, N
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       COMPLEX(KIND=WP)         A( LDA, * ), WORK( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLARGE pre- and post-multiplies a complex general n by n matrix A
!> with a random unitary matrix: A = U*D*U'.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.
!> \endverbatim
!>
!> \param[in,out] A
!> \verbatim
!>          A is COMPLEX(KIND=WP) array, dimension (LDA,N)
!>          On entry, the original n by n matrix A.
!>          On exit, A is overwritten by U*A*U' for some random
!>          unitary matrix U.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= N.
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
!> \param[out] WORK
!> \verbatim
!>          WORK is COMPLEX(KIND=WP) array, dimension (2*N)
!> \endverbatim
!>
!> \param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
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
!> \ingroup complex16_matgen
!
!  =====================================================================
SUBROUTINE XLARGE(N, A, LDA, ISEED, WORK, INFO)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: LDA, N
  INTEGER, INTENT(OUT) :: INFO
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
  COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,*)
  COMPLEX(KIND=WP), INTENT(OUT) :: WORK(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(KIND=WP), PARAMETER :: QZERO = 0.0E+0_WP
  COMPLEX(KIND=WP), PARAMETER :: ZERO = (QZERO, QZERO), ONE = (1.0E+0_WP, QZERO)
!     ..
!     .. Local Scalars ..
  INTEGER :: I
  REAL(KIND=WP) :: WN
  COMPLEX(KIND=WP) :: TAU, WA, WB
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA, XGEMV, XGERC, XLARNV, XSCAL
!     ..
!     .. External Functions ..
  REAL(KIND=WP), EXTERNAL :: QXNRM2
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
  INFO = 0
  IF (N .LT. 0) THEN
     INFO = -1
  ELSE IF (LDA .LT. MAX(1, N)) THEN
     INFO = -3
  END IF
  IF (INFO .LT. 0) THEN
     CALL XERBLA('XLARGE', -INFO)
     RETURN
  END IF
!
!     pre- and post-multiply A by random unitary matrix
!
  DO I = N, 1, -1
!
!        generate random reflection
!
     CALL XLARNV(3, ISEED, N-I+1, WORK)
     WN = QXNRM2(N-I+1, WORK, 1)
     WA = (WN / ABS(WORK(1))) * WORK(1)
     IF (WN .EQ. QZERO) THEN
        TAU = ZERO
     ELSE
        WB = WORK(1) + WA
        CALL XSCAL(N-I, ONE / WB, WORK(2), 1)
        WORK(1) = ONE
        TAU = REAL(WB / WA, WP)
     END IF
!
!        multiply A(i:n,1:n) by random reflection from the left
!
     CALL XGEMV('C', N-I+1, N, ONE, A(I,1), LDA, WORK, 1, ZERO, WORK(N+1), 1)
     CALL XGERC(N-I+1, N, -TAU, WORK, 1, WORK(N+1), 1, A(I,1), LDA)
!
!        multiply A(1:n,i:n) by random reflection from the right
!
     CALL XGEMV('N', N, N-I+1, ONE, A(1,I), LDA, WORK, 1, ZERO, WORK(N+1), 1)
     CALL XGERC(N, N-I+1, -TAU, WORK(N+1), 1, WORK, 1, A(1,I), LDA)
  END DO
!
!     End of XLARGE
!
END SUBROUTINE XLARGE

!> \brief \b XLAGHE
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE XLAGHE( N, K, D, A, LDA, ISEED, WORK, INFO )
!
!       .. Scalar Arguments ..
!       INTEGER            INFO, K, LDA, N
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       REAL(WP)           D( * )
!       COMPLEX(WP)        A( LDA, * ), WORK( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLAGHE generates a complex hermitian matrix A, by pre- and post-
!> multiplying a real diagonal matrix D with a random unitary matrix:
!> A = U*D*U'. The semi-bandwidth may then be reduced to k by additional
!> unitary transformations.
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
!> \param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of nonzero subdiagonals within the band of A.
!>          0 <= K <= N-1.
!> \endverbatim
!>
!> \param[in] D
!> \verbatim
!>          D is REAL(WP) array, dimension (N)
!>          The diagonal elements of the diagonal matrix D.
!> \endverbatim
!>
!> \param[out] A
!> \verbatim
!>          A is COMPLEX(WP) array, dimension (LDA,N)
!>          The generated n by n hermitian matrix A (the full matrix is
!>          stored).
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
!>          WORK is COMPLEX(WP) array, dimension (2*N)
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
SUBROUTINE XLAGHE(N, K, D, A, LDA, ISEED, WORK, INFO)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016

  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: N, K, LDA
  INTEGER, INTENT(OUT) :: INFO
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
  REAL(WP), INTENT(IN) :: D(*)
  COMPLEX(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  COMPLEX(WP), PARAMETER :: ZERO = (0.0E+0_WP, 0.0E+0_WP), ONE = (1.0E+0_WP, 0.0E+0_WP), HALF = (0.5E+0_WP, 0.0E+0_WP)
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J
  REAL(WP) :: WN
  COMPLEX(WP) :: ALPHA, TAU, WA, WB
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA, XAXPY, XGEMV, XGERC, XHEMV, XHER2, XLARNV, XSCAL
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QXNRM2
  COMPLEX(WP), EXTERNAL :: XDOTC
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
  INFO = 0
  IF (N .LT. 0) THEN
     INFO = -1
  ELSE IF ((K .LT. 0) .OR. (K .GT. (N-1))) THEN
     INFO = -2
  ELSE IF (LDA .LT. MAX(1, N)) THEN
     INFO = -5
  END IF
  IF (INFO .LT. 0) THEN
     CALL XERBLA('XLAGHE', -INFO)
     RETURN
  END IF
!
!     initialize lower triangle of A to diagonal matrix
!
  DO J = 1, N
     DO I = J+1, N
        A(I,J) = ZERO
     END DO
  END DO
  DO I = 1, N
     A(I,I) = D(I)
  END DO
!
!     Generate lower triangle of hermitian matrix
!
  DO I = N-1, 1, -1
!
!        generate random reflection
!
     CALL XLARNV(3, ISEED, N-I+1, WORK)
     WN = QXNRM2(N-I+1, WORK, 1)
     WA = (WN / ABS(WORK(1))) * WORK(1)
     IF (WN .EQ. ZERO) THEN
        TAU = ZERO
     ELSE
        WB = WORK(1) + WA
        CALL XSCAL(N-I, ONE / WB, WORK(2), 1)
        WORK(1) = ONE
        TAU = REAL(WB / WA, WP)
     END IF
!
!        apply random reflection to A(i:n,i:n) from the left
!        and the right
!
!        compute  y := tau * A * u
!
     CALL XHEMV('L', N-I+1, TAU, A(I,I), LDA, WORK, 1, ZERO, WORK(N+1), 1)
!
!        compute  v := y - 1/2 * tau * ( y, u ) * u
!
     ALPHA = -HALF*TAU*XDOTC(N-I+1, WORK(N+1), 1, WORK, 1)
     CALL XAXPY(N-I+1, ALPHA, WORK, 1, WORK(N+1), 1)
!
!        apply the transformation as a rank-2 update to A(i:n,i:n)
!
     CALL XHER2('L', N-I+1, -ONE, WORK, 1, WORK(N+1), 1, A(I,I), LDA)
  END DO
!
!     Reduce number of subdiagonals to K
!
  DO I = 1, N-1-K
!
!        generate reflection to annihilate A(k+i+1:n,i)
!
     WN = QXNRM2(N-K-I+1, A(K+I,I), 1)
     WA = (WN / ABS(A(K+I,I))) * A(K+I,I)
     IF (WN .EQ. ZERO) THEN
        TAU = ZERO
     ELSE
        WB = A(K+I,I) + WA
        CALL XSCAL(N-K-I, ONE / WB, A(K+I+1,I), 1)
        A(K+I,I) = ONE
        TAU = REAL(WB / WA, WP)
     END IF
!
!        apply reflection to A(k+i:n,i+1:k+i-1) from the left
!
     CALL XGEMV('C', N-K-I+1, K-1, ONE, A(K+I,I+1), LDA, A(K+I,I), 1, ZERO, WORK, 1)
     CALL XGERC(N-K-I+1, K-1, -TAU, A(K+I,I), 1, WORK, 1, A(K+I,I+1), LDA)
!
!        apply reflection to A(k+i:n,k+i:n) from the left and the right
!
!        compute  y := tau * A * u
!
     CALL XHEMV('L', N-K-I+1, TAU, A(K+I,K+I), LDA, A(K+I,I), 1, ZERO, WORK, 1)
!
!        compute  v := y - 1/2 * tau * ( y, u ) * u
!
     ALPHA = -HALF*TAU*XDOTC( N-K-I+1, WORK, 1, A(K+I,I), 1)
     CALL XAXPY(N-K-I+1, ALPHA, A(K+I,I), 1, WORK, 1)
!
!        apply hermitian rank-2 update to A(k+i:n,k+i:n)
!
     CALL XHER2('L', N-K-I+1, -ONE, A(K+I,I), 1, WORK, 1, A(K+I,K+I), LDA)
!
     A(K+I,I) = -WA
     DO J = K+I+1, N
        A(J,I) = ZERO
     END DO
  END DO
!
!     Store full hermitian matrix
!
  DO J = 1, N
     DO I = J+1, N
        A(J,I) = CONJG(A(I,J))
     END DO
  END DO
!
!     End of XLAGHE
!
END SUBROUTINE XLAGHE

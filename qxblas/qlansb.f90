!> \brief \b QLANSB returns the value of the 1-norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a symmetric band matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLANSB + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlansb.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlansb.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlansb.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QLANSB( NORM, UPLO, N, K, AB, LDAB, WORK )
! 
!       .. Scalar Arguments ..
!       CHARACTER          NORM, UPLO
!       INTEGER            K, LDAB, N
!       ..
!       .. Array Arguments ..
!       REAL(WP)           AB( LDAB, * ), WORK( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLANSB  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the element of  largest absolute value  of an
!> n by n symmetric band matrix A,  with k super-diagonals.
!> \endverbatim
!>
!> \return QLANSB
!> \verbatim
!>
!>    QLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!>             (
!>             ( norm1(A),         NORM = '1', 'O' or 'o'
!>             (
!>             ( normI(A),         NORM = 'I' or 'i'
!>             (
!>             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!>
!> where  norm1  denotes the  one norm of a matrix (maximum column sum),
!> normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!> normF  denotes the  Frobenius norm of a matrix (square root of sum of
!> squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] NORM
!> \verbatim
!>          NORM is CHARACTER*1
!>          Specifies the value to be returned in QLANSB as described
!>          above.
!> \endverbatim
!>
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies whether the upper or lower triangular part of the
!>          band matrix A is supplied.
!>          = 'U':  Upper triangular part is supplied
!>          = 'L':  Lower triangular part is supplied
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.  When N = 0, QLANSB is
!>          set to zero.
!> \endverbatim
!>
!> \param[in] K
!> \verbatim
!>          K is INTEGER
!>          The number of super-diagonals or sub-diagonals of the
!>          band matrix A.  K >= 0.
!> \endverbatim
!>
!> \param[in] AB
!> \verbatim
!>          AB is REAL(WP) array, dimension (LDAB,N)
!>          The upper or lower triangle of the symmetric band matrix A,
!>          stored in the first K+1 rows of AB.  The j-th column of A is
!>          stored in the j-th column of the array AB as follows:
!>          if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
!>          if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
!> \endverbatim
!>
!> \param[in] LDAB
!> \verbatim
!>          LDAB is INTEGER
!>          The leading dimension of the array AB.  LDAB >= K+1.
!> \endverbatim
!>
!> \param[out] WORK
!> \verbatim
!>          WORK is REAL(WP) array, dimension (MAX(1,LWORK)),
!>          where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
!>          WORK is not referenced.
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
!> \ingroup realOTHERauxiliary
!
!  =====================================================================
FUNCTION QLANSB( NORM, UPLO, N, K, AB, LDAB, WORK )
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: NORM, UPLO
  INTEGER, INTENT(IN) :: K, LDAB, N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: AB(LDAB,*)
  REAL(WP), INTENT(OUT) :: WORK(*)
!     ..
!
  REAL(WP) :: QLANSB
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J, L
  REAL(WP) :: ABSA, SCAL, SUM, VAL
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QLASSQ
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME, QISNAN
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS, SQRT
  INTEGER, INTRINSIC :: MAX, MIN
!     ..
!     .. Executable Statements ..
!
  IF (N .EQ. 0) THEN
     VAL = ZERO
  ELSE IF (LSAME(NORM,'M')) THEN
!
!        Find max(abs(A(i,j))).
!
     VAL = ZERO
     IF (LSAME(UPLO,'U')) THEN
        DO J = 1, N
           DO I = MAX(K+2-J, 1), K + 1
              SUM = ABS(AB(I,J))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
        END DO
     ELSE
        DO J = 1, N
           DO I = 1, MIN(N+1-J, K+1)
              SUM = ABS(AB(I,J))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
        END DO
     END IF
  ELSE IF (LSAME(NORM,'I') .OR. LSAME(NORM,'O') .OR. (NORM .EQ. '1')) THEN
!
!        Find normI(A) ( = norm1(A), since A is symmetric).
!
     VAL = ZERO
     IF (LSAME(UPLO,'U')) THEN
        DO J = 1, N
           SUM = ZERO
           L = K + 1 - J
           DO I = MAX(1,J-K), J - 1
              ABSA = ABS(AB(L+I,J))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
           END DO
           WORK(J) = SUM + ABS(AB(K+1,J))
        END DO
        DO I = 1, N
           SUM = WORK(I)
           IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
        END DO
     ELSE
        DO I = 1, N
           WORK(I) = ZERO
        END DO
        DO J = 1, N
           SUM = WORK(J) + ABS(AB(1,J))
           L = 1 - J
           DO I = J + 1, MIN(N,J+K)
              ABSA = ABS(AB(L+I,J))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
           END DO
           IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
        END DO
     END IF
  ELSE IF (LSAME( NORM, 'F' ) .OR. LSAME(NORM,'E')) THEN
!
!        Find normF(A).
!
     SCAL = ZERO
     SUM = ONE
     IF (K .GT. 0) THEN
        IF (LSAME(UPLO,'U')) THEN
           DO J = 2, N
              CALL QLASSQ(MIN(J-1,K), AB(MAX(K+2-J,1),J), 1, SCAL, SUM)
           END DO
           L = K + 1
        ELSE
           DO J = 1, N - 1
              CALL QLASSQ(MIN(N-J,K), AB(2,J), 1, SCAL, SUM)
           END DO
           L = 1
        END IF
        SUM = 2*SUM
     ELSE
        L = 1
     END IF
     CALL QLASSQ(N, AB(L,1), LDAB, SCAL, SUM)
     VAL = SCAL*SQRT(SUM)
  END IF
!
  QLANSB = VAL
!
!     End of QLANSB
!
END FUNCTION QLANSB

!> \brief \b QLANSY returns the value of the 1-norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a real symmetric matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLANSY + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlansy.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlansy.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlansy.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QLANSY( NORM, UPLO, N, A, LDA, WORK )
! 
!       .. Scalar Arguments ..
!       CHARACTER          NORM, UPLO
!       INTEGER            LDA, N
!       ..
!       .. Array Arguments ..
!       REAL(WP)           A( LDA, * ), WORK( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLANSY  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> real symmetric matrix A.
!> \endverbatim
!>
!> \return QLANSY
!> \verbatim
!>
!>    QLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
!>          Specifies the value to be returned in QLANSY as described
!>          above.
!> \endverbatim
!>
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies whether the upper or lower triangular part of the
!>          symmetric matrix A is to be referenced.
!>          = 'U':  Upper triangular part of A is referenced
!>          = 'L':  Lower triangular part of A is referenced
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.  When N = 0, QLANSY is
!>          set to zero.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is REAL(WP) array, dimension (LDA,N)
!>          The symmetric matrix A.  If UPLO = 'U', the leading n by n
!>          upper triangular part of A contains the upper triangular part
!>          of the matrix A, and the strictly lower triangular part of A
!>          is not referenced.  If UPLO = 'L', the leading n by n lower
!>          triangular part of A contains the lower triangular part of
!>          the matrix A, and the strictly upper triangular part of A is
!>          not referenced.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(N,1).
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
!> \date November 2015
!
!> \ingroup realSYauxiliary
!
!  =====================================================================
FUNCTION QLANSY(NORM, UPLO, N, A, LDA, WORK)
!
!  -- LAPACK auxiliary routine (version 3.6.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2015
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: NORM, UPLO
  INTEGER, INTENT(IN) :: LDA, N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: A(LDA,*)
  REAL(WP), INTENT(OUT) :: WORK(*)
!     ..
!
  REAL(WP) :: QLANSY
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J
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
!     ..
!     .. Executable Statements ..
!
  IF (N .EQ. 0) THEN
     VAL = ZERO
  ELSE IF (LSAME(NORM, 'M')) THEN
!
!        Find max(abs(A(i,j))).
!
     VAL = ZERO
     IF (LSAME(UPLO, 'U')) THEN
        DO J = 1, N
           DO I = 1, J
              SUM = ABS(A(I,J))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
        END DO
     ELSE
        DO J = 1, N
           DO I = J, N
              SUM = ABS(A(I,J))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
        END DO
     END IF
  ELSE IF (LSAME(NORM, 'I') .OR. LSAME(NORM, 'O') .OR. (NORM .EQ. '1')) THEN
!
!        Find normI(A) ( = norm1(A), since A is symmetric).
!
     VAL = ZERO
     IF (LSAME(UPLO,'U')) THEN
        DO J = 1, N
           SUM = ZERO
           DO I = 1, J - 1
              ABSA = ABS(A(I,J))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
           END DO
           WORK(J) = SUM + ABS(A(J,J))
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
           SUM = WORK(J) + ABS(A(J,J))
           DO I = J + 1, N
              ABSA = ABS(A(I,J))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
           END DO
           IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
        END DO
     END IF
  ELSE IF (LSAME(NORM, 'F') .OR. LSAME(NORM, 'E')) THEN
!
!        Find normF(A).
!
     SCAL = ZERO
     SUM = ONE
     IF (LSAME(UPLO, 'U')) THEN
        DO J = 2, N
           CALL QLASSQ(J-1, A(1,J), 1, SCAL, SUM)
        END DO
     ELSE
        DO J = 1, N - 1
           CALL QLASSQ(N-J, A(J+1,J), 1, SCAL, SUM)
        END DO
     END IF
     SUM = 2*SUM
     CALL QLASSQ(N, A, LDA+1, SCAL, SUM)
     VAL = SCAL*SQRT(SUM)
  END IF
!
  QLANSY = VAL
!
!     End of QLANSY
!
END FUNCTION QLANSY

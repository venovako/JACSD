!> \brief \b QLANGE returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLANGE + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlange.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlange.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlange.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP)           FUNCTION QLANGE( NORM, M, N, A, LDA, WORK )
! 
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            LDA, M, N
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
!> QLANGE  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> real matrix A.
!> \endverbatim
!>
!> \return QLANGE
!> \verbatim
!>
!>    QLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
!>          Specifies the value to be returned in QLANGE as described
!>          above.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.  M >= 0.  When M = 0,
!>          QLANGE is set to zero.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.  N >= 0.  When N = 0,
!>          QLANGE is set to zero.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is REAL(WP) array, dimension (LDA,N)
!>          The m by n matrix A.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(M,1).
!> \endverbatim
!>
!> \param[out] WORK
!> \verbatim
!>          WORK is REAL(WP) array, dimension (MAX(1,LWORK)),
!>          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
!>          referenced.
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
!> \ingroup realGEauxiliary
!
!  =====================================================================
FUNCTION QLANGE( NORM, M, N, A, LDA, WORK )
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: NORM
  INTEGER, INTENT(IN) :: LDA, M, N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: A( LDA, * )
  REAL(WP), INTENT(OUT) :: WORK( * )
!     ..
  REAL(WP) :: QLANGE
!
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J
  REAL(WP) :: SCAL, SUM, VAL, TEMP
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QLASSQ
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME, QISNAN
!     ..
!     .. Executable Statements ..
!
  IF (MIN(M,N) .EQ. 0) THEN
     VAL = ZERO
  ELSE IF (LSAME(NORM, 'M')) THEN
!
!        Find max(abs(A(i,j))).
!
     VAL = ZERO
     DO J = 1, N
        DO I = 1, M
           TEMP = ABS(A(I,J)) 
           IF ((VAL .LT. TEMP) .OR. QISNAN(TEMP)) VAL = TEMP
        END DO
     END DO
  ELSE IF (LSAME(NORM, 'O') .OR. (NORM .EQ. '1')) THEN
!
!        Find norm1(A).
!
     VAL = ZERO
     DO J = 1, N
        SUM = ZERO
        DO I = 1, M
           SUM = SUM + ABS(A(I,J))
        END DO
        IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
     END DO
  ELSE IF (LSAME(NORM, 'I')) THEN
!
!        Find normI(A).
!
     DO I = 1, M
        WORK(I) = ZERO
     END DO
     DO J = 1, N
        DO I = 1, M
           WORK(I) = WORK(I) + ABS(A(I,J))
        END DO
     END DO
     VAL = ZERO
     DO I = 1, M
        TEMP = WORK(I)
        IF ((VAL .LT.TEMP) .OR. QISNAN(TEMP)) VAL = TEMP
     END DO
  ELSE IF (LSAME(NORM, 'F') .OR. LSAME(NORM, 'E')) THEN
!
!        Find normF(A).
!
     SCAL = ZERO
     SUM = ONE
     DO J = 1, N
        CALL QLASSQ(M, A(1,J), 1, SCAL, SUM)
     END DO
     VAL = SCAL*SQRT(SUM)
  END IF
!
  QLANGE = VAL
!
!     End of QANGE
!
END FUNCTION QLANGE

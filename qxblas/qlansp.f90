!> \brief \b QLANSP returns the value of the 1-norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a symmetric matrix supplied in packed form.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLANSP + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlansp.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlansp.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlansp.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QLANSP( NORM, UPLO, N, AP, WORK )
! 
!       .. Scalar Arguments ..
!       CHARACTER          NORM, UPLO
!       INTEGER            N
!       ..
!       .. Array Arguments ..
!       REAL(WP)           AP( * ), WORK( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLANSP  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> real symmetric matrix A,  supplied in packed form.
!> \endverbatim
!>
!> \return QLANSP
!> \verbatim
!>
!>    QLANSP = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
!>          Specifies the value to be returned in QLANSP as described
!>          above.
!> \endverbatim
!>
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies whether the upper or lower triangular part of the
!>          symmetric matrix A is supplied.
!>          = 'U':  Upper triangular part of A is supplied
!>          = 'L':  Lower triangular part of A is supplied
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.  When N = 0, QLANSP is
!>          set to zero.
!> \endverbatim
!>
!> \param[in] AP
!> \verbatim
!>          AP is REAL(WP) array, dimension (N*(N+1)/2)
!>          The upper or lower triangle of the symmetric matrix A, packed
!>          columnwise in a linear array.  The j-th column of A is stored
!>          in the array AP as follows:
!>          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
!>          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
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
FUNCTION QLANSP( NORM, UPLO, N, AP, WORK )
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
  INTEGER, INTENT(IN) :: N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: AP(*)
  REAL(WP), INTENT(OUT) :: WORK(*)
!     ..
  REAL(WP) :: QLANSP
!
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J, K
  REAL(WP) :: ABSA, SCAL, SUM, VAL
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QLASSQ
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME, QISNAN
!     ..
!     .. Intrinsic Functions ..
#ifndef USE_IBM
  REAL(WP), INTRINSIC :: ABS, SQRT
#endif
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
        K = 1
        DO J = 1, N
           DO I = K, K + J - 1
              SUM = ABS(AP(I))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
           K = K + J
        END DO
     ELSE
        K = 1
        DO J = 1, N
           DO I = K, K + N - J
              SUM = ABS(AP(I))
              IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
           END DO
           K = K + N - J + 1
        END DO
     END IF
  ELSE IF (LSAME(NORM,'I') .OR. LSAME(NORM,'O') .OR. (NORM .EQ. '1')) THEN
!
!        Find normI(A) ( = norm1(A), since A is symmetric).
!
     VAL = ZERO
     K = 1
     IF (LSAME(UPLO,'U')) THEN
        DO J = 1, N
           SUM = ZERO
           DO I = 1, J - 1
              ABSA = ABS(AP(K))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
              K = K + 1
           END DO
           WORK(J) = SUM + ABS(AP(K))
           K = K + 1
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
           SUM = WORK(J) + ABS(AP(K))
           K = K + 1
           DO I = J + 1, N
              ABSA = ABS(AP(K))
              SUM = SUM + ABSA
              WORK(I) = WORK(I) + ABSA
              K = K + 1
           END DO
           IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM               
        END DO
     END IF
  ELSE IF (LSAME(NORM,'F') .OR. LSAME(NORM,'E')) THEN
!
!        Find normF(A).
!
     SCAL = ZERO
     SUM = ONE
     K = 2
     IF (LSAME(UPLO,'U')) THEN
        DO J = 2, N
           CALL QLASSQ(J-1, AP(K), 1, SCAL, SUM)
           K = K + J
        END DO
     ELSE
        DO J = 1, N - 1
           CALL QLASSQ(N-J, AP(K), 1, SCAL, SUM)
           K = K + N - J + 1
        END DO
     END IF
     SUM = 2*SUM
     K = 1
     DO I = 1, N
        IF (AP(K) .NE. ZERO) THEN
           ABSA = ABS(AP(K))
           IF (SCAL .LT. ABSA) THEN
              SUM = ONE + SUM * (SCAL/ABSA)**2
              SCAL = ABSA
           ELSE
              SUM = SUM + (ABSA/SCAL)**2
           END IF
        END IF
        IF (LSAME(UPLO,'U')) THEN
           K = K + I + 1
        ELSE
           K = K + N - I + 1
        END IF
     END DO
     VAL = SCAL*SQRT(SUM)
  END IF
!
  QLANSP = VAL
!
!     End of QLANSP
!
END FUNCTION QLANSP

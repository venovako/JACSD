!> \brief \b QLANGB returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of general band matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLANGB + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlangb.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlangb.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlangb.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
! 
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            KL, KU, LDAB, N
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
!> QLANGB  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the element of  largest absolute value  of an
!> n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
!> \endverbatim
!>
!> \return QLANGB
!> \verbatim
!>
!>    QLANGB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
!>          Specifies the value to be returned in QLANGB as described
!>          above.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the matrix A.  N >= 0.  When N = 0, QLANGB is
!>          set to zero.
!> \endverbatim
!>
!> \param[in] KL
!> \verbatim
!>          KL is INTEGER
!>          The number of sub-diagonals of the matrix A.  KL >= 0.
!> \endverbatim
!>
!> \param[in] KU
!> \verbatim
!>          KU is INTEGER
!>          The number of super-diagonals of the matrix A.  KU >= 0.
!> \endverbatim
!>
!> \param[in] AB
!> \verbatim
!>          AB is REAL(WP) array, dimension (LDAB,N)
!>          The band matrix A, stored in rows 1 to KL+KU+1.  The j-th
!>          column of A is stored in the j-th column of the array AB as
!>          follows:
!>          AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(n,j+kl).
!> \endverbatim
!>
!> \param[in] LDAB
!> \verbatim
!>          LDAB is INTEGER
!>          The leading dimension of the array AB.  LDAB >= KL+KU+1.
!> \endverbatim
!>
!> \param[out] WORK
!> \verbatim
!>          WORK is REAL(WP) array, dimension (MAX(1,LWORK)),
!>          where LWORK >= N when NORM = 'I'; otherwise, WORK is not
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
!> \ingroup realGBauxiliary
!
!  =====================================================================
FUNCTION QLANGB(NORM, N, KL, KU, AB, LDAB, WORK)
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
  INTEGER, INTENT(IN) :: KL, KU, LDAB, N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: AB(LDAB,*)
  REAL(WP), INTENT(OUT) :: WORK(*)
!     ..
  REAL(WP) :: QLANGB
!
! =====================================================================
!
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: I, J, K, L
  REAL(WP) :: SCAL, SUM, VAL, TEMP
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
  ELSE IF (LSAME(NORM, 'M')) THEN
!
!        Find max(abs(A(i,j))).
!
     VAL = ZERO
     DO J = 1, N
        DO I = MAX(KU+2-J, 1), MIN(N+KU+1-J, KL+KU+1)
           TEMP = ABS(AB(I, J)) 
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
        DO I = MAX(KU+2-J, 1), MIN(N+KU+1-J, KL+KU+1)
           SUM = SUM + ABS(AB(I, J))
        END DO
        IF ((VAL .LT. SUM) .OR. QISNAN(SUM)) VAL = SUM
     END DO
  ELSE IF (LSAME(NORM, 'I')) THEN
!
!        Find normI(A).
!
     DO I = 1, N
        WORK(I) = ZERO
     END DO
     DO J = 1, N
        K = KU + 1 - J
        DO I = MAX(1, J-KU), MIN(N, J+KL)
           WORK(I) = WORK(I) + ABS(AB(K+I, J))
        END DO
     END DO
     VAL = ZERO
     DO I = 1, N
        TEMP = WORK(I)
        IF ((VAL .LT. TEMP) .OR. QISNAN(TEMP)) VAL = TEMP
     END DO
  ELSE IF (LSAME(NORM, 'F') .OR. LSAME(NORM, 'E')) THEN
!
!        Find normF(A).
!
     SCAL = ZERO
     SUM = ONE
     DO J = 1, N
        L = MAX( 1, J-KU )
        K = KU + 1 - J + L
        CALL QLASSQ(MIN(N, J+KL)-L+1, AB(K, J), 1, SCAL, SUM)
     END DO
     VAL = SCAL*SQRT(SUM)
  END IF
!
  QLANGB = VAL
!
!     End of QLANGB
!
END FUNCTION QLANGB

!> \brief \b QSYRK
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
! 
!       .. Scalar Arguments ..
!       REAL(WP) ALPHA,BETA
!       INTEGER K,LDA,LDC,N
!       CHARACTER TRANS,UPLO
!       ..
!       .. Array Arguments ..
!       REAL(WP) A(LDA,*),C(LDC,*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QSYRK  performs one of the symmetric rank k operations
!>
!>    C := alpha*A*A**T + beta*C,
!>
!> or
!>
!>    C := alpha*A**T*A + beta*C,
!>
!> where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!> and  A  is an  n by k  matrix in the first case and a  k by n  matrix
!> in the second case.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!>           triangular  part  of the  array  C  is to be  referenced  as
!>           follows:
!>
!>              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!>                                  is to be referenced.
!>
!>              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!>                                  is to be referenced.
!> \endverbatim
!>
!> \param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>           On entry,  TRANS  specifies the operation to be performed as
!>           follows:
!>
!>              TRANS = 'N' or 'n'   C := alpha*A*A**T + beta*C.
!>
!>              TRANS = 'T' or 't'   C := alpha*A**T*A + beta*C.
!>
!>              TRANS = 'C' or 'c'   C := alpha*A**T*A + beta*C.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry,  N specifies the order of the matrix C.  N must be
!>           at least zero.
!> \endverbatim
!>
!> \param[in] K
!> \verbatim
!>          K is INTEGER
!>           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!>           of  columns   of  the   matrix   A,   and  on   entry   with
!>           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!>           of rows of the matrix  A.  K must be at least zero.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is REAL(WP)
!>           On entry, ALPHA specifies the scalar alpha.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is REAL(WP) array of DIMENSION ( LDA, ka ), where ka is
!>           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!>           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!>           part of the array  A  must contain the matrix  A,  otherwise
!>           the leading  k by n  part of the array  A  must contain  the
!>           matrix A.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>           On entry, LDA specifies the first dimension of A as declared
!>           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!>           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!>           be at least  max( 1, k ).
!> \endverbatim
!>
!> \param[in] BETA
!> \verbatim
!>          BETA is REAL(WP)
!>           On entry, BETA specifies the scalar beta.
!> \endverbatim
!>
!> \param[in,out] C
!> \verbatim
!>          C is REAL(WP) array of DIMENSION ( LDC, n ).
!>           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!>           upper triangular part of the array C must contain the upper
!>           triangular part  of the  symmetric matrix  and the strictly
!>           lower triangular part of C is not referenced.  On exit, the
!>           upper triangular part of the array  C is overwritten by the
!>           upper triangular part of the updated matrix.
!>           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!>           lower triangular part of the array C must contain the lower
!>           triangular part  of the  symmetric matrix  and the strictly
!>           upper triangular part of C is not referenced.  On exit, the
!>           lower triangular part of the array  C is overwritten by the
!>           lower triangular part of the updated matrix.
!> \endverbatim
!>
!> \param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>           On entry, LDC specifies the first dimension of C as declared
!>           in  the  calling  (sub)  program.   LDC  must  be  at  least
!>           max( 1, n ).
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
!> \ingroup single_blas_level3
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Level 3 Blas routine.
!>
!>  -- Written on 8-February-1989.
!>     Jack Dongarra, Argonne National Laboratory.
!>     Iain Duff, AERE Harwell.
!>     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!>     Sven Hammarling, Numerical Algorithms Group Ltd.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE QSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
!
!  -- Reference BLAS level3 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: ALPHA,BETA
  INTEGER, INTENT(IN) :: K,LDA,LDC,N
  CHARACTER, INTENT(IN) :: TRANS,UPLO
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: A(LDA,*)
  REAL(WP), INTENT(INOUT) :: C(LDC,*)
!     ..
!
!  =====================================================================
!
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Intrinsic Functions ..
  INTEGER, INTRINSIC :: MAX
!     ..
!     .. Local Scalars ..
  REAL(WP) :: TEMP
  INTEGER :: I,INFO,J,L,NROWA
  LOGICAL :: UPPER
!     ..
!     .. Parameters ..
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!
!     Test the input parameters.
!
  IF (LSAME(TRANS,'N')) THEN
     NROWA = N
  ELSE
     NROWA = K
  END IF
  UPPER = LSAME(UPLO,'U')
!
  INFO = 0
  IF ((.NOT. UPPER) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 1
  ELSE IF ((.NOT. LSAME(TRANS,'N')) .AND. (.NOT. LSAME(TRANS,'T')) .AND. (.NOT. LSAME(TRANS,'C'))) THEN
     INFO = 2
  ELSE IF (N .LT. 0) THEN
     INFO = 3
  ELSE IF (K .LT. 0) THEN
     INFO = 4
  ELSE IF (LDA .LT. MAX(1,NROWA)) THEN
     INFO = 7
  ELSE IF (LDC .LT. MAX(1,N)) THEN
     INFO = 10
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('QSYRK ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((N .EQ. 0) .OR. (((ALPHA .EQ. ZERO) .OR. (K .EQ. 0)) .AND. (BETA .EQ. ONE))) RETURN
!
!     And when  alpha.eq.zero.
!
  IF (ALPHA .EQ. ZERO) THEN
     IF (UPPER) THEN
        IF (BETA .EQ. ZERO) THEN
           DO J = 1,N
              DO I = 1,J
                 C(I,J) = ZERO
              END DO
           END DO
        ELSE
           DO J = 1,N
              DO I = 1,J
                 C(I,J) = BETA*C(I,J)
              END DO
           END DO
        END IF
     ELSE
        IF (BETA .EQ. ZERO) THEN
           DO J = 1,N
              DO I = J,N
                 C(I,J) = ZERO
              END DO
           END DO
        ELSE
           DO J = 1,N
              DO I = J,N
                 C(I,J) = BETA*C(I,J)
              END DO
           END DO
        END IF
     END IF
     RETURN
  END IF
!
!     Start the operations.
!
  IF (LSAME(TRANS,'N')) THEN
!
!        Form  C := alpha*A*A**T + beta*C.
!
     IF (UPPER) THEN
        DO J = 1,N
           IF (BETA .EQ. ZERO) THEN
              DO I = 1,J
                 C(I,J) = ZERO
              END DO
           ELSE IF (BETA .NE. ONE) THEN
              DO I = 1,J
                 C(I,J) = BETA*C(I,J)
              END DO
           END IF
           DO L = 1,K
              IF (A(J,L) .NE. ZERO) THEN
                 TEMP = ALPHA*A(J,L)
                 DO I = 1,J
                    C(I,J) = C(I,J) + TEMP*A(I,L)
                 END DO
              END IF
           END DO
        END DO
     ELSE
        DO J = 1,N
           IF (BETA .EQ. ZERO) THEN
              DO I = J,N
                 C(I,J) = ZERO
              END DO
           ELSE IF (BETA .NE. ONE) THEN
              DO I = J,N
                 C(I,J) = BETA*C(I,J)
              END DO
           END IF
           DO L = 1,K
              IF (A(J,L) .NE. ZERO) THEN
                 TEMP = ALPHA*A(J,L)
                 DO I = J,N
                    C(I,J) = C(I,J) + TEMP*A(I,L)
                 END DO
              END IF
           END DO
        END DO
     END IF
  ELSE
!
!        Form  C := alpha*A**T*A + beta*C.
!
     IF (UPPER) THEN
        DO J = 1,N
           DO I = 1,J
              TEMP = ZERO
              DO L = 1,K
                 TEMP = TEMP + A(L,I)*A(L,J)
              END DO
              IF (BETA .EQ. ZERO) THEN
                 C(I,J) = ALPHA*TEMP
              ELSE
                 C(I,J) = ALPHA*TEMP + BETA*C(I,J)
              END IF
           END DO
        END DO
     ELSE
        DO J = 1,N
           DO I = J,N
              TEMP = ZERO
              DO L = 1,K
                 TEMP = TEMP + A(L,I)*A(L,J)
              END DO
              IF (BETA .EQ. ZERO) THEN
                 C(I,J) = ALPHA*TEMP
              ELSE
                 C(I,J) = ALPHA*TEMP + BETA*C(I,J)
              END IF
           END DO
        END DO
     END IF
  END IF
!
!     End of QSYRK .
!
END SUBROUTINE QSYRK

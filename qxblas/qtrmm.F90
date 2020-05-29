!> \brief \b QTRMM
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
! 
!       .. Scalar Arguments ..
!       REAL(WP) ALPHA
!       INTEGER LDA,LDB,M,N
!       CHARACTER DIAG,SIDE,TRANSA,UPLO
!       ..
!       .. Array Arguments ..
!       REAL(WP) A(LDA,*),B(LDB,*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QTRMM  performs one of the matrix-matrix operations
!>
!>    B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
!>
!> where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!> non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!>
!>    op( A ) = A   or   op( A ) = A**T.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>           On entry,  SIDE specifies whether  op( A ) multiplies B from
!>           the left or right as follows:
!>
!>              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!>
!>              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!> \endverbatim
!>
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On entry, UPLO specifies whether the matrix A is an upper or
!>           lower triangular matrix as follows:
!>
!>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!>
!>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!> \endverbatim
!>
!> \param[in] TRANSA
!> \verbatim
!>          TRANSA is CHARACTER*1
!>           On entry, TRANSA specifies the form of op( A ) to be used in
!>           the matrix multiplication as follows:
!>
!>              TRANSA = 'N' or 'n'   op( A ) = A.
!>
!>              TRANSA = 'T' or 't'   op( A ) = A**T.
!>
!>              TRANSA = 'C' or 'c'   op( A ) = A**T.
!> \endverbatim
!>
!> \param[in] DIAG
!> \verbatim
!>          DIAG is CHARACTER*1
!>           On entry, DIAG specifies whether or not A is unit triangular
!>           as follows:
!>
!>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!>
!>              DIAG = 'N' or 'n'   A is not assumed to be unit
!>                                  triangular.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>           On entry, M specifies the number of rows of B. M must be at
!>           least zero.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry, N specifies the number of columns of B.  N must be
!>           at least zero.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is REAL(WP)
!>           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!>           zero then  A is not referenced and  B need not be set before
!>           entry.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is REAL(WP) array of DIMENSION ( LDA, k ), where k is m
!>           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!>           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!>           upper triangular part of the array  A must contain the upper
!>           triangular matrix  and the strictly lower triangular part of
!>           A is not referenced.
!>           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!>           lower triangular part of the array  A must contain the lower
!>           triangular matrix  and the strictly upper triangular part of
!>           A is not referenced.
!>           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!>           A  are not referenced either,  but are assumed to be  unity.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>           On entry, LDA specifies the first dimension of A as declared
!>           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!>           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!>           then LDA must be at least max( 1, n ).
!> \endverbatim
!>
!> \param[in,out] B
!> \verbatim
!>          B is REAL(WP) array of DIMENSION ( LDB, n ).
!>           Before entry,  the leading  m by n part of the array  B must
!>           contain the matrix  B,  and  on exit  is overwritten  by the
!>           transformed matrix.
!> \endverbatim
!>
!> \param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>           On entry, LDB specifies the first dimension of B as declared
!>           in  the  calling  (sub)  program.   LDB  must  be  at  least
!>           max( 1, m ).
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
SUBROUTINE QTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
!
!  -- Reference BLAS level3 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: ALPHA
  INTEGER, INTENT(IN) :: LDA,LDB,M,N
  CHARACTER, INTENT(IN) :: DIAG,SIDE,TRANSA,UPLO
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: A(LDA,*)
  REAL(WP), INTENT(INOUT) :: B(LDB,*)
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
!     .. Local Scalars ..
  REAL(WP) :: TEMP
  INTEGER :: I,INFO,J,K,NROWA
  LOGICAL :: LSIDE,NOUNIT,UPPER
!     ..
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!
!     Test the input parameters.
!
  LSIDE = LSAME(SIDE,'L')
  IF (LSIDE) THEN
     NROWA = M
  ELSE
     NROWA = N
  END IF
  NOUNIT = LSAME(DIAG,'N')
  UPPER = LSAME(UPLO,'U')
!
  INFO = 0
  IF ((.NOT. LSIDE) .AND. (.NOT. LSAME(SIDE,'R'))) THEN
     INFO = 1
  ELSE IF ((.NOT. UPPER) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 2
  ELSE IF ((.NOT. LSAME(TRANSA,'N')) .AND. (.NOT. LSAME(TRANSA,'T')) .AND. (.NOT. LSAME(TRANSA,'C'))) THEN
     INFO = 3
  ELSE IF ((.NOT. LSAME(DIAG,'U')) .AND. (.NOT. LSAME(DIAG,'N'))) THEN
     INFO = 4
  ELSE IF (M .LT. 0) THEN
     INFO = 5
  ELSE IF (N .LT. 0) THEN
     INFO = 6
  ELSE IF (LDA .LT. MAX(1,NROWA)) THEN
     INFO = 9
  ELSE IF (LDB .LT. MAX(1,M)) THEN
     INFO = 11
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('QTRMM ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((M .EQ. 0) .OR. (N .EQ. 0)) RETURN
!
!     And when  alpha.eq.zero.
!
  IF (ALPHA .EQ. ZERO) THEN
     DO J = 1,N
        DO I = 1,M
           B(I,J) = ZERO
        END DO
     END DO
     RETURN
  END IF
!
!     Start the operations.
!
  IF (LSIDE) THEN
     IF (LSAME(TRANSA,'N')) THEN
!
!           Form  B := alpha*A*B.
!
        IF (UPPER) THEN
           DO J = 1,N
              DO K = 1,M
                 IF (B(K,J) .NE. ZERO) THEN
                    TEMP = ALPHA*B(K,J)
                    DO I = 1,K - 1
                       B(I,J) = B(I,J) + TEMP*A(I,K)
                    END DO
                    IF (NOUNIT) TEMP = TEMP*A(K,K)
                    B(K,J) = TEMP
                 END IF
              END DO
           END DO
        ELSE
           DO J = 1,N
              DO K = M,1,-1
                 IF (B(K,J) .NE. ZERO) THEN
                    TEMP = ALPHA*B(K,J)
                    B(K,J) = TEMP
                    IF (NOUNIT) B(K,J) = B(K,J)*A(K,K)
                    DO I = K + 1,M
                       B(I,J) = B(I,J) + TEMP*A(I,K)
                    END DO
                 END IF
              END DO
           END DO
        END IF
     ELSE
!
!           Form  B := alpha*A**T*B.
!
        IF (UPPER) THEN
           DO J = 1,N
              DO I = M,1,-1
                 TEMP = B(I,J)
                 IF (NOUNIT) TEMP = TEMP*A(I,I)
                 DO K = 1,I - 1
                    TEMP = TEMP + A(K,I)*B(K,J)
                 END DO
                 B(I,J) = ALPHA*TEMP
              END DO
           END DO
        ELSE
           DO J = 1,N
              DO I = 1,M
                 TEMP = B(I,J)
                 IF (NOUNIT) TEMP = TEMP*A(I,I)
                 DO K = I + 1,M
                    TEMP = TEMP + A(K,I)*B(K,J)
                 END DO
                 B(I,J) = ALPHA*TEMP
              END DO
           END DO
        END IF
     END IF
  ELSE
     IF (LSAME(TRANSA,'N')) THEN
!
!           Form  B := alpha*B*A.
!
        IF (UPPER) THEN
           DO J = N,1,-1
              TEMP = ALPHA
              IF (NOUNIT) TEMP = TEMP*A(J,J)
              DO I = 1,M
                 B(I,J) = TEMP*B(I,J)
              END DO
              DO K = 1,J - 1
                 IF (A(K,J) .NE. ZERO) THEN
                    TEMP = ALPHA*A(K,J)
                    DO I = 1,M
                       B(I,J) = B(I,J) + TEMP*B(I,K)
                    END DO
                 END IF
              END DO
           END DO
        ELSE
           DO J = 1,N
              TEMP = ALPHA
              IF (NOUNIT) TEMP = TEMP*A(J,J)
              DO I = 1,M
                 B(I,J) = TEMP*B(I,J)
              END DO
              DO K = J + 1,N
                 IF (A(K,J) .NE. ZERO) THEN
                    TEMP = ALPHA*A(K,J)
                    DO I = 1,M
                       B(I,J) = B(I,J) + TEMP*B(I,K)
                    END DO
                 END IF
              END DO
           END DO
        END IF
     ELSE
!
!           Form  B := alpha*B*A**T.
!
        IF (UPPER) THEN
           DO K = 1,N
              DO J = 1,K - 1
                 IF (A(J,K) .NE. ZERO) THEN
                    TEMP = ALPHA*A(J,K)
                    DO I = 1,M
                       B(I,J) = B(I,J) + TEMP*B(I,K)
                    END DO
                 END IF
              END DO
              TEMP = ALPHA
              IF (NOUNIT) TEMP = TEMP*A(K,K)
              IF (TEMP .NE. ONE) THEN
                 DO I = 1,M
                    B(I,K) = TEMP*B(I,K)
                 END DO
              END IF
           END DO
        ELSE
           DO K = N,1,-1
              DO J = K + 1,N
                 IF (A(J,K) .NE. ZERO) THEN
                    TEMP = ALPHA*A(J,K)
                    DO I = 1,M
                       B(I,J) = B(I,J) + TEMP*B(I,K)
                    END DO
                 END IF
              END DO
              TEMP = ALPHA
              IF (NOUNIT) TEMP = TEMP*A(K,K)
              IF (TEMP .NE. ONE) THEN
                 DO I = 1,M
                    B(I,K) = TEMP*B(I,K)
                 END DO
              END IF
           END DO
        END IF
     END IF
  END IF
!
!     End of QTRMM .
!
END SUBROUTINE QTRMM

!> \brief \b XTBSV
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,K,LDA,N
!       CHARACTER DIAG,TRANS,UPLO
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) A(LDA,*),X(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XTBSV  solves one of the systems of equations
!>
!>    A*x = b,   or   A**T*x = b,   or   A**H*x = b,
!>
!> where b and x are n element vectors and A is an n by n unit, or
!> non-unit, upper or lower triangular band matrix, with ( k + 1 )
!> diagonals.
!>
!> No test for singularity or near-singularity is included in this
!> routine. Such tests must be performed before calling this routine.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On entry, UPLO specifies whether the matrix is an upper or
!>           lower triangular matrix as follows:
!>
!>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!>
!>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!> \endverbatim
!>
!> \param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>           On entry, TRANS specifies the equations to be solved as
!>           follows:
!>
!>              TRANS = 'N' or 'n'   A*x = b.
!>
!>              TRANS = 'T' or 't'   A**T*x = b.
!>
!>              TRANS = 'C' or 'c'   A**H*x = b.
!> \endverbatim
!>
!> \param[in] DIAG
!> \verbatim
!>          DIAG is CHARACTER*1
!>           On entry, DIAG specifies whether or not A is unit
!>           triangular as follows:
!>
!>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!>
!>              DIAG = 'N' or 'n'   A is not assumed to be unit
!>                                  triangular.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry, N specifies the order of the matrix A.
!>           N must be at least zero.
!> \endverbatim
!>
!> \param[in] K
!> \verbatim
!>          K is INTEGER
!>           On entry with UPLO = 'U' or 'u', K specifies the number of
!>           super-diagonals of the matrix A.
!>           On entry with UPLO = 'L' or 'l', K specifies the number of
!>           sub-diagonals of the matrix A.
!>           K must satisfy  0 .le. K.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is COMPLEX(WP) array of DIMENSION ( LDA, n ).
!>           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
!>           by n part of the array A must contain the upper triangular
!>           band part of the matrix of coefficients, supplied column by
!>           column, with the leading diagonal of the matrix in row
!>           ( k + 1 ) of the array, the first super-diagonal starting at
!>           position 2 in row k, and so on. The top left k by k triangle
!>           of the array A is not referenced.
!>           The following program segment will transfer an upper
!>           triangular band matrix from conventional full matrix storage
!>           to band storage:
!>
!>                 DO 20, J = 1, N
!>                    M = K + 1 - J
!>                    DO 10, I = MAX( 1, J - K ), J
!>                       A( M + I, J ) = matrix( I, J )
!>              10    CONTINUE
!>              20 CONTINUE
!>
!>           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
!>           by n part of the array A must contain the lower triangular
!>           band part of the matrix of coefficients, supplied column by
!>           column, with the leading diagonal of the matrix in row 1 of
!>           the array, the first sub-diagonal starting at position 1 in
!>           row 2, and so on. The bottom right k by k triangle of the
!>           array A is not referenced.
!>           The following program segment will transfer a lower
!>           triangular band matrix from conventional full matrix storage
!>           to band storage:
!>
!>                 DO 20, J = 1, N
!>                    M = 1 - J
!>                    DO 10, I = J, MIN( N, J + K )
!>                       A( M + I, J ) = matrix( I, J )
!>              10    CONTINUE
!>              20 CONTINUE
!>
!>           Note that when DIAG = 'U' or 'u' the elements of the array A
!>           corresponding to the diagonal elements of the matrix are not
!>           referenced, but are assumed to be unity.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>           On entry, LDA specifies the first dimension of A as declared
!>           in the calling (sub) program. LDA must be at least
!>           ( k + 1 ).
!> \endverbatim
!>
!> \param[in,out] X
!> \verbatim
!>          X is COMPLEX(WP) array of dimension at least
!>           ( 1 + ( n - 1 )*abs( INCX ) ).
!>           Before entry, the incremented array X must contain the n
!>           element right-hand side vector b. On exit, X is overwritten
!>           with the solution vector x.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>           On entry, INCX specifies the increment for the elements of
!>           X. INCX must not be zero.
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
!> \ingroup complex_blas_level2
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Level 2 Blas routine.
!>
!>  -- Written on 22-October-1986.
!>     Jack Dongarra, Argonne National Lab.
!>     Jeremy Du Croz, Nag Central Office.
!>     Sven Hammarling, Nag Central Office.
!>     Richard Hanson, Sandia National Labs.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE XTBSV(UPLO,TRANS,DIAG,N,K,A,LDA,X,INCX)
!
!  -- Reference BLAS level2 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,K,LDA,N
  CHARACTER, INTENT(IN) :: DIAG,TRANS,UPLO
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(IN) :: A(LDA,*)
  COMPLEX(WP), INTENT(INOUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  COMPLEX(WP), PARAMETER :: ZERO = (0.0E+0_WP,0.0E+0_WP)
!     ..
!     .. Local Scalars ..
  COMPLEX(WP) :: TEMP
  INTEGER :: I,INFO,IX,J,JX,KPLUS1,KX,L
  LOGICAL :: NOCONJ,NOUNIT
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Intrinsic Functions ..
  COMPLEX(WP), INTRINSIC :: CONJG
  INTEGER, INTRINSIC :: MAX,MIN
!     ..
!
!     Test the input parameters.
!
  INFO = 0
  IF ((.NOT. LSAME(UPLO,'U')) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 1
  ELSE IF ((.NOT. LSAME(TRANS,'N')) .AND. (.NOT. LSAME(TRANS,'T')) .AND. (.NOT. LSAME(TRANS,'C'))) THEN
     INFO = 2
  ELSE IF ((.NOT. LSAME(DIAG,'U')) .AND. (.NOT. LSAME(DIAG,'N'))) THEN
     INFO = 3
  ELSE IF (N .LT. 0) THEN
     INFO = 4
  ELSE IF (K .LT. 0) THEN
     INFO = 5
  ELSE IF (LDA .LT. (K+1)) THEN
     INFO = 7
  ELSE IF (INCX .EQ. 0) THEN
     INFO = 9
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('XTBSV ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF (N .EQ. 0) RETURN
!
  NOCONJ = LSAME(TRANS,'T')
  NOUNIT = LSAME(DIAG,'N')
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
  IF (INCX .LE. 0) THEN
     KX = 1 - (N-1)*INCX
  ELSE IF (INCX .NE. 1) THEN
     KX = 1
  END IF
!
!     Start the operations. In this version the elements of A are
!     accessed by sequentially with one pass through A.
!
  IF (LSAME(TRANS,'N')) THEN
!
!        Form  x := inv( A )*x.
!
     IF (LSAME(UPLO,'U')) THEN
        KPLUS1 = K + 1
        IF (INCX .EQ. 1) THEN
           DO J = N,1,-1
              IF (X(J) .NE. ZERO) THEN
                 L = KPLUS1 - J
                 IF (NOUNIT) X(J) = X(J)/A(KPLUS1,J)
                 TEMP = X(J)
                 DO I = J - 1,MAX(1,J-K),-1
                    X(I) = X(I) - TEMP*A(L+I,J)
                 END DO
              END IF
           END DO
        ELSE
           KX = KX + (N-1)*INCX
           JX = KX
           DO J = N,1,-1
              KX = KX - INCX
              IF (X(JX) .NE. ZERO) THEN
                 IX = KX
                 L = KPLUS1 - J
                 IF (NOUNIT) X(JX) = X(JX)/A(KPLUS1,J)
                 TEMP = X(JX)
                 DO I = J - 1,MAX(1,J-K),-1
                    X(IX) = X(IX) - TEMP*A(L+I,J)
                    IX = IX - INCX
                 END DO
              END IF
              JX = JX - INCX
           END DO
        END IF
     ELSE
        IF (INCX .EQ. 1) THEN
           DO J = 1,N
              IF (X(J) .NE. ZERO) THEN
                 L = 1 - J
                 IF (NOUNIT) X(J) = X(J)/A(1,J)
                 TEMP = X(J)
                 DO I = J + 1,MIN(N,J+K)
                    X(I) = X(I) - TEMP*A(L+I,J)
                 END DO
              END IF
           END DO
        ELSE
           JX = KX
           DO J = 1,N
              KX = KX + INCX
              IF (X(JX) .NE. ZERO) THEN
                 IX = KX
                 L = 1 - J
                 IF (NOUNIT) X(JX) = X(JX)/A(1,J)
                 TEMP = X(JX)
                 DO I = J + 1,MIN(N,J+K)
                    X(IX) = X(IX) - TEMP*A(L+I,J)
                    IX = IX + INCX
                 END DO
              END IF
              JX = JX + INCX
           END DO
        END IF
     END IF
  ELSE
!
!        Form  x := inv( A**T )*x  or  x := inv( A**H )*x.
!
     IF (LSAME(UPLO,'U')) THEN
        KPLUS1 = K + 1
        IF (INCX .EQ. 1) THEN
           DO J = 1,N
              TEMP = X(J)
              L = KPLUS1 - J
              IF (NOCONJ) THEN
                 DO I = MAX(1,J-K),J - 1
                    TEMP = TEMP - A(L+I,J)*X(I)
                 END DO
                 IF (NOUNIT) TEMP = TEMP/A(KPLUS1,J)
              ELSE
                 DO I = MAX(1,J-K),J - 1
                    TEMP = TEMP - CONJG(A(L+I,J))*X(I)
                 END DO
                 IF (NOUNIT) TEMP = TEMP/CONJG(A(KPLUS1,J))
              END IF
              X(J) = TEMP
           END DO
        ELSE
           JX = KX
           DO J = 1,N
              TEMP = X(JX)
              IX = KX
              L = KPLUS1 - J
              IF (NOCONJ) THEN
                 DO I = MAX(1,J-K),J - 1
                    TEMP = TEMP - A(L+I,J)*X(IX)
                    IX = IX + INCX
                 END DO
                 IF (NOUNIT) TEMP = TEMP/A(KPLUS1,J)
              ELSE
                 DO I = MAX(1,J-K),J - 1
                    TEMP = TEMP - CONJG(A(L+I,J))*X(IX)
                    IX = IX + INCX
                 END DO
                 IF (NOUNIT) TEMP = TEMP/CONJG(A(KPLUS1,J))
              END IF
              X(JX) = TEMP
              JX = JX + INCX
              IF (J .GT. K) KX = KX + INCX
           END DO
        END IF
     ELSE
        IF (INCX .EQ. 1) THEN
           DO J = N,1,-1
              TEMP = X(J)
              L = 1 - J
              IF (NOCONJ) THEN
                 DO I = MIN(N,J+K),J + 1,-1
                    TEMP = TEMP - A(L+I,J)*X(I)
                 END DO
                 IF (NOUNIT) TEMP = TEMP/A(1,J)
              ELSE
                 DO I = MIN(N,J+K),J + 1,-1
                    TEMP = TEMP - CONJG(A(L+I,J))*X(I)
                 END DO
                 IF (NOUNIT) TEMP = TEMP/CONJG(A(1,J))
              END IF
              X(J) = TEMP
           END DO
        ELSE
           KX = KX + (N-1)*INCX
           JX = KX
           DO J = N,1,-1
              TEMP = X(JX)
              IX = KX
              L = 1 - J
              IF (NOCONJ) THEN
                 DO I = MIN(N,J+K),J + 1,-1
                    TEMP = TEMP - A(L+I,J)*X(IX)
                    IX = IX - INCX
                 END DO
                 IF (NOUNIT) TEMP = TEMP/A(1,J)
              ELSE
                 DO I = MIN(N,J+K),J + 1,-1
                    TEMP = TEMP - CONJG(A(L+I,J))*X(IX)
                    IX = IX - INCX
                 END DO
                 IF (NOUNIT) TEMP = TEMP/CONJG(A(1,J))
              END IF
              X(JX) = TEMP
              JX = JX - INCX
              IF ((N-J) .GE. K) KX = KX - INCX
           END DO
        END IF
     END IF
  END IF
!
!     End of XTBSV .
!
END SUBROUTINE XTBSV

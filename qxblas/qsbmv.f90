!> \brief \b QSBMV
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
! 
!       .. Scalar Arguments ..
!       REAL(WP) ALPHA,BETA
!       INTEGER INCX,INCY,K,LDA,N
!       CHARACTER UPLO
!       ..
!       .. Array Arguments ..
!       REAL(WP) A(LDA,*),X(*),Y(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QSBMV  performs the matrix-vector  operation
!>
!>    y := alpha*A*x + beta*y,
!>
!> where alpha and beta are scalars, x and y are n element vectors and
!> A is an n by n symmetric band matrix, with k super-diagonals.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On entry, UPLO specifies whether the upper or lower
!>           triangular part of the band matrix A is being supplied as
!>           follows:
!>
!>              UPLO = 'U' or 'u'   The upper triangular part of A is
!>                                  being supplied.
!>
!>              UPLO = 'L' or 'l'   The lower triangular part of A is
!>                                  being supplied.
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
!>           On entry, K specifies the number of super-diagonals of the
!>           matrix A. K must satisfy  0 .le. K.
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
!>          A is REAL(WP) array of DIMENSION ( LDA, n ).
!>           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
!>           by n part of the array A must contain the upper triangular
!>           band part of the symmetric matrix, supplied column by
!>           column, with the leading diagonal of the matrix in row
!>           ( k + 1 ) of the array, the first super-diagonal starting at
!>           position 2 in row k, and so on. The top left k by k triangle
!>           of the array A is not referenced.
!>           The following program segment will transfer the upper
!>           triangular part of a symmetric band matrix from conventional
!>           full matrix storage to band storage:
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
!>           band part of the symmetric matrix, supplied column by
!>           column, with the leading diagonal of the matrix in row 1 of
!>           the array, the first sub-diagonal starting at position 1 in
!>           row 2, and so on. The bottom right k by k triangle of the
!>           array A is not referenced.
!>           The following program segment will transfer the lower
!>           triangular part of a symmetric band matrix from conventional
!>           full matrix storage to band storage:
!>
!>                 DO 20, J = 1, N
!>                    M = 1 - J
!>                    DO 10, I = J, MIN( N, J + K )
!>                       A( M + I, J ) = matrix( I, J )
!>              10    CONTINUE
!>              20 CONTINUE
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
!> \param[in] X
!> \verbatim
!>          X is REAL(WP) array of DIMENSION at least
!>           ( 1 + ( n - 1 )*abs( INCX ) ).
!>           Before entry, the incremented array X must contain the
!>           vector x.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>           On entry, INCX specifies the increment for the elements of
!>           X. INCX must not be zero.
!> \endverbatim
!>
!> \param[in] BETA
!> \verbatim
!>          BETA is REAL(WP)
!>           On entry, BETA specifies the scalar beta.
!> \endverbatim
!>
!> \param[in,out] Y
!> \verbatim
!>          Y is REAL(WP) array of DIMENSION at least
!>           ( 1 + ( n - 1 )*abs( INCY ) ).
!>           Before entry, the incremented array Y must contain the
!>           vector y. On exit, Y is overwritten by the updated vector y.
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>           On entry, INCY specifies the increment for the elements of
!>           Y. INCY must not be zero.
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
!> \ingroup single_blas_level2
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Level 2 Blas routine.
!>  The vector and matrix arguments are not referenced when N = 0, or M = 0
!>
!>  -- Written on 22-October-1986.
!>     Jack Dongarra, Argonne National Lab.
!>     Jeremy Du Croz, Nag Central Office.
!>     Sven Hammarling, Nag Central Office.
!>     Richard Hanson, Sandia National Labs.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE QSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
!
!  -- Reference BLAS level2 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: ALPHA,BETA
  INTEGER, INTENT(IN) :: INCX,INCY,K,LDA,N
  CHARACTER, INTENT(IN) :: UPLO
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: A(LDA,*),X(*)
  REAL(WP), INTENT(INOUT) :: Y(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: TEMP1,TEMP2
  INTEGER :: I,INFO,IX,IY,J,JX,JY,KPLUS1,KX,KY,L
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Intrinsic Functions ..
  INTEGER, INTRINSIC :: MAX,MIN
!     ..
!
!     Test the input parameters.
!
  INFO = 0
  IF ((.NOT. LSAME(UPLO,'U')) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 1
  ELSE IF (N .LT. 0) THEN
     INFO = 2
  ELSE IF (K .LT. 0) THEN
     INFO = 3
  ELSE IF (LDA .LT. (K+1)) THEN
     INFO = 6
  ELSE IF (INCX .EQ. 0) THEN
     INFO = 8
  ELSE IF (INCY.EQ.0) THEN
     INFO = 11
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('QSBMV ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((N .EQ. 0) .OR. ((ALPHA .EQ. ZERO) .AND. (BETA .EQ. ONE))) RETURN
!
!     Set up the start points in  X  and  Y.
!
  IF (INCX .GT. 0) THEN
     KX = 1
  ELSE
     KX = 1 - (N-1)*INCX
  END IF
  IF (INCY .GT. 0) THEN
     KY = 1
  ELSE
     KY = 1 - (N-1)*INCY
  END IF
!
!     Start the operations. In this version the elements of the array A
!     are accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
  IF (BETA .NE. ONE) THEN
     IF (INCY .EQ. 1) THEN
        IF (BETA .EQ. ZERO) THEN
           DO I = 1,N
              Y(I) = ZERO
           END DO
        ELSE
           DO I = 1,N
              Y(I) = BETA*Y(I)
           END DO
        END IF
     ELSE
        IY = KY
        IF (BETA .EQ. ZERO) THEN
           DO I = 1,N
              Y(IY) = ZERO
              IY = IY + INCY
           END DO
        ELSE
           DO I = 1,N
              Y(IY) = BETA*Y(IY)
              IY = IY + INCY
           END DO
        END IF
     END IF
  END IF
  IF (ALPHA .EQ. ZERO) RETURN
  IF (LSAME(UPLO,'U')) THEN
!
!        Form  y  when upper triangle of A is stored.
!
     KPLUS1 = K + 1
     IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
        DO J = 1,N
           TEMP1 = ALPHA*X(J)
           TEMP2 = ZERO
           L = KPLUS1 - J
           DO I = MAX(1,J-K),J - 1
              Y(I) = Y(I) + TEMP1*A(L+I,J)
              TEMP2 = TEMP2 + A(L+I,J)*X(I)
           END DO
           Y(J) = Y(J) + TEMP1*A(KPLUS1,J) + ALPHA*TEMP2
        END DO
     ELSE
        JX = KX
        JY = KY
        DO J = 1,N
           TEMP1 = ALPHA*X(JX)
           TEMP2 = ZERO
           IX = KX
           IY = KY
           L = KPLUS1 - J
           DO I = MAX(1,J-K),J - 1
              Y(IY) = Y(IY) + TEMP1*A(L+I,J)
              TEMP2 = TEMP2 + A(L+I,J)*X(IX)
              IX = IX + INCX
              IY = IY + INCY
           END DO
           Y(JY) = Y(JY) + TEMP1*A(KPLUS1,J) + ALPHA*TEMP2
           JX = JX + INCX
           JY = JY + INCY
           IF (J .GT. K) THEN
              KX = KX + INCX
              KY = KY + INCY
           END IF
        END DO
     END IF
  ELSE
!
!        Form  y  when lower triangle of A is stored.
!
     IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
        DO J = 1,N
           TEMP1 = ALPHA*X(J)
           TEMP2 = ZERO
           Y(J) = Y(J) + TEMP1*A(1,J)
           L = 1 - J
           DO I = J + 1,MIN(N,J+K)
              Y(I) = Y(I) + TEMP1*A(L+I,J)
              TEMP2 = TEMP2 + A(L+I,J)*X(I)
           END DO
           Y(J) = Y(J) + ALPHA*TEMP2
        END DO
     ELSE
        JX = KX
        JY = KY
        DO J = 1,N
           TEMP1 = ALPHA*X(JX)
           TEMP2 = ZERO
           Y(JY) = Y(JY) + TEMP1*A(1,J)
           L = 1 - J
           IX = JX
           IY = JY
           DO I = J + 1,MIN(N,J+K)
              IX = IX + INCX
              IY = IY + INCY
              Y(IY) = Y(IY) + TEMP1*A(L+I,J)
              TEMP2 = TEMP2 + A(L+I,J)*X(IX)
           END DO
           Y(JY) = Y(JY) + ALPHA*TEMP2
           JX = JX + INCX
           JY = JY + INCY
        END DO
     END IF
  END IF
!
!     End of QSBMV .
!
END SUBROUTINE QSBMV

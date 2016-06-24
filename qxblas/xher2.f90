!> \brief \b XHER2
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) ALPHA
!       INTEGER INCX,INCY,LDA,N
!       CHARACTER UPLO
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) A(LDA,*),X(*),Y(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XHER2  performs the hermitian rank 2 operation
!>
!>    A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
!>
!> where alpha is a scalar, x and y are n element vectors and A is an n
!> by n hermitian matrix.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On entry, UPLO specifies whether the upper or lower
!>           triangular part of the array A is to be referenced as
!>           follows:
!>
!>              UPLO = 'U' or 'u'   Only the upper triangular part of A
!>                                  is to be referenced.
!>
!>              UPLO = 'L' or 'l'   Only the lower triangular part of A
!>                                  is to be referenced.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry, N specifies the order of the matrix A.
!>           N must be at least zero.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX(WP)
!>           On entry, ALPHA specifies the scalar alpha.
!> \endverbatim
!>
!> \param[in] X
!> \verbatim
!>          X is COMPLEX(WP) array of dimension at least
!>           ( 1 + ( n - 1 )*abs( INCX ) ).
!>           Before entry, the incremented array X must contain the n
!>           element vector x.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>           On entry, INCX specifies the increment for the elements of
!>           X. INCX must not be zero.
!> \endverbatim
!>
!> \param[in] Y
!> \verbatim
!>          Y is COMPLEX(WP) array of dimension at least
!>           ( 1 + ( n - 1 )*abs( INCY ) ).
!>           Before entry, the incremented array Y must contain the n
!>           element vector y.
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>           On entry, INCY specifies the increment for the elements of
!>           Y. INCY must not be zero.
!> \endverbatim
!>
!> \param[in,out] A
!> \verbatim
!>          A is COMPLEX(WP) array of DIMENSION ( LDA, n ).
!>           Before entry with  UPLO = 'U' or 'u', the leading n by n
!>           upper triangular part of the array A must contain the upper
!>           triangular part of the hermitian matrix and the strictly
!>           lower triangular part of A is not referenced. On exit, the
!>           upper triangular part of the array A is overwritten by the
!>           upper triangular part of the updated matrix.
!>           Before entry with UPLO = 'L' or 'l', the leading n by n
!>           lower triangular part of the array A must contain the lower
!>           triangular part of the hermitian matrix and the strictly
!>           upper triangular part of A is not referenced. On exit, the
!>           lower triangular part of the array A is overwritten by the
!>           lower triangular part of the updated matrix.
!>           Note that the imaginary parts of the diagonal elements need
!>           not be set, they are assumed to be zero, and on exit they
!>           are set to zero.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>           On entry, LDA specifies the first dimension of A as declared
!>           in the calling (sub) program. LDA must be at least
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
SUBROUTINE XHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
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
  COMPLEX(WP), INTENT(IN) :: ALPHA
  INTEGER, INTENT(IN) :: INCX,INCY,LDA,N
  CHARACTER, INTENT(IN) :: UPLO
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
  COMPLEX(WP), INTENT(IN) :: X(*),Y(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  COMPLEX(WP), PARAMETER :: ZERO = (0.0E+0_WP,0.0E+0_WP)
!     ..
!     .. Local Scalars ..
  COMPLEX(WP) :: TEMP1,TEMP2
  INTEGER :: I,INFO,IX,IY,J,JX,JY,KX,KY
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Intrinsic Functions ..
  COMPLEX(WP), INTRINSIC :: CONJG
  INTEGER, INTRINSIC :: MAX
  REAL(WP), INTRINSIC :: REAL
!     ..
!
!     Test the input parameters.
!
  INFO = 0
  IF ((.NOT. LSAME(UPLO,'U')) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 1
  ELSE IF (N .LT. 0) THEN
     INFO = 2
  ELSE IF (INCX .EQ. 0) THEN
     INFO = 5
  ELSE IF (INCY .EQ. 0) THEN
     INFO = 7
  ELSE IF (LDA .LT. MAX(1,N)) THEN
     INFO = 9
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('XHER2 ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((N .EQ. 0) .OR. (ALPHA .EQ. ZERO)) RETURN
!
!     Set up the start points in X and Y if the increments are not both
!     unity.
!
  IF ((INCX .NE. 1) .OR. (INCY .NE. 1)) THEN
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
     JX = KX
     JY = KY
  END IF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.
!
  IF (LSAME(UPLO,'U')) THEN
!
!        Form  A  when A is stored in the upper triangle.
!
     IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
        DO J = 1,N
           IF ((X(J) .NE. ZERO) .OR. (Y(J) .NE. ZERO)) THEN
              TEMP1 = ALPHA*CONJG(Y(J))
              TEMP2 = CONJG(ALPHA*X(J))
              DO I = 1,J - 1
                 A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
              END DO
              A(J,J) = REAL(A(J,J)) + REAL(X(J)*TEMP1+Y(J)*TEMP2)
           ELSE
              A(J,J) = REAL(A(J,J))
           END IF
        END DO
     ELSE
        DO J = 1,N
           IF ((X(JX) .NE. ZERO) .OR. (Y(JY) .NE. ZERO)) THEN
              TEMP1 = ALPHA*CONJG(Y(JY))
              TEMP2 = CONJG(ALPHA*X(JX))
              IX = KX
              IY = KY
              DO I = 1,J - 1
                 A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                 IX = IX + INCX
                 IY = IY + INCY
              END DO
              A(J,J) = REAL(A(J,J)) + REAL(X(JX)*TEMP1+Y(JY)*TEMP2)
           ELSE
              A(J,J) = REAL(A(J,J))
           END IF
           JX = JX + INCX
           JY = JY + INCY
        END DO
     END IF
  ELSE
!
!        Form  A  when A is stored in the lower triangle.
!
     IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
        DO J = 1,N
           IF ((X(J) .NE. ZERO) .OR. (Y(J) .NE. ZERO)) THEN
              TEMP1 = ALPHA*CONJG(Y(J))
              TEMP2 = CONJG(ALPHA*X(J))
              A(J,J) = REAL(A(J,J)) + REAL(X(J)*TEMP1+Y(J)*TEMP2)
              DO I = J + 1,N
                 A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
              END DO
           ELSE
              A(J,J) = REAL(A(J,J))
           END IF
        END DO
     ELSE
        DO J = 1,N
           IF ((X(JX) .NE. ZERO) .OR. (Y(JY) .NE. ZERO)) THEN
              TEMP1 = ALPHA*CONJG(Y(JY))
              TEMP2 = CONJG(ALPHA*X(JX))
              A(J,J) = REAL(A(J,J)) + REAL(X(JX)*TEMP1+Y(JY)*TEMP2)
              IX = JX
              IY = JY
              DO I = J + 1,N
                 IX = IX + INCX
                 IY = IY + INCY
                 A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
              END DO
           ELSE
              A(J,J) = REAL(A(J,J))
           END IF
           JX = JX + INCX
           JY = JY + INCY
        END DO
     END IF
  END IF
!
!     End of XHER2 .
!
END SUBROUTINE XHER2

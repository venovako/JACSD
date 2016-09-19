!> \brief \b XHPR
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XHPR(UPLO,N,ALPHA,X,INCX,AP)
! 
!       .. Scalar Arguments ..
!       REAL(WP) ALPHA
!       INTEGER INCX,N
!       CHARACTER UPLO
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) AP(*),X(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XHPR    performs the hermitian rank 1 operation
!>
!>    A := alpha*x*x**H + A,
!>
!> where alpha is a real scalar, x is an n element vector and A is an
!> n by n hermitian matrix, supplied in packed form.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On entry, UPLO specifies whether the upper or lower
!>           triangular part of the matrix A is supplied in the packed
!>           array AP as follows:
!>
!>              UPLO = 'U' or 'u'   The upper triangular part of A is
!>                                  supplied in AP.
!>
!>              UPLO = 'L' or 'l'   The lower triangular part of A is
!>                                  supplied in AP.
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
!>          ALPHA is REAL(WP)
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
!> \param[in,out] AP
!> \verbatim
!>          AP is COMPLEX(WP) array of DIMENSION at least
!>           ( ( n*( n + 1 ) )/2 ).
!>           Before entry with  UPLO = 'U' or 'u', the array AP must
!>           contain the upper triangular part of the hermitian matrix
!>           packed sequentially, column by column, so that AP( 1 )
!>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
!>           and a( 2, 2 ) respectively, and so on. On exit, the array
!>           AP is overwritten by the upper triangular part of the
!>           updated matrix.
!>           Before entry with UPLO = 'L' or 'l', the array AP must
!>           contain the lower triangular part of the hermitian matrix
!>           packed sequentially, column by column, so that AP( 1 )
!>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
!>           and a( 3, 1 ) respectively, and so on. On exit, the array
!>           AP is overwritten by the lower triangular part of the
!>           updated matrix.
!>           Note that the imaginary parts of the diagonal elements need
!>           not be set, they are assumed to be zero, and on exit they
!>           are set to zero.
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
SUBROUTINE XHPR(UPLO,N,ALPHA,X,INCX,AP)
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
  REAL(WP), INTENT(IN) :: ALPHA
  INTEGER, INTENT(IN) :: INCX,N
  CHARACTER, INTENT(IN) :: UPLO
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(INOUT) :: AP(*)
  COMPLEX(WP), INTENT(IN) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: RZERO = 0.0E+0_WP
  COMPLEX(WP), PARAMETER :: ZERO = (0.0E+0_WP,0.0E+0_WP)
!     ..
!     .. Local Scalars ..
  COMPLEX(WP) :: TEMP
  INTEGER :: I,INFO,IX,J,JX,K,KK,KX
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Intrinsic Functions ..
#ifndef USE_IBM
  COMPLEX(WP), INTRINSIC :: CONJG
  REAL(WP), INTRINSIC :: REAL
#endif
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
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('XHPR  ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((N .EQ. 0) .OR. (ALPHA .EQ. RZERO)) RETURN
!
!     Set the start point in X if the increment is not unity.
!
  IF (INCX .LE. 0) THEN
     KX = 1 - (N-1)*INCX
  ELSE IF (INCX .NE. 1) THEN
     KX = 1
  END IF
!
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.
!
  KK = 1
  IF (LSAME(UPLO,'U')) THEN
!
!        Form  A  when upper triangle is stored in AP.
!
     IF (INCX .EQ. 1) THEN
        DO J = 1,N
           IF (X(J) .NE. ZERO) THEN
              TEMP = ALPHA*CONJG(X(J))
              K = KK
              DO I = 1,J - 1
                 AP(K) = AP(K) + X(I)*TEMP
                 K = K + 1
              END DO
              AP(KK+J-1) = REAL(AP(KK+J-1)) + REAL(X(J)*TEMP)
           ELSE
              AP(KK+J-1) = REAL(AP(KK+J-1))
           END IF
           KK = KK + J
        END DO
     ELSE
        JX = KX
        DO J = 1,N
           IF (X(JX) .NE. ZERO) THEN
              TEMP = ALPHA*CONJG(X(JX))
              IX = KX
              DO K = KK,KK + J - 2
                 AP(K) = AP(K) + X(IX)*TEMP
                 IX = IX + INCX
              END DO
              AP(KK+J-1) = REAL(AP(KK+J-1)) + REAL(X(JX)*TEMP)
           ELSE
              AP(KK+J-1) = REAL(AP(KK+J-1))
           END IF
           JX = JX + INCX
           KK = KK + J
        END DO
     END IF
  ELSE
!
!        Form  A  when lower triangle is stored in AP.
!
     IF (INCX .EQ. 1) THEN
        DO J = 1,N
           IF (X(J) .NE. ZERO) THEN
              TEMP = ALPHA*CONJG(X(J))
              AP(KK) = REAL(AP(KK)) + REAL(TEMP*X(J))
              K = KK + 1
              DO I = J + 1,N
                 AP(K) = AP(K) + X(I)*TEMP
                 K = K + 1
              END DO
           ELSE
              AP(KK) = REAL(AP(KK))
           END IF
           KK = KK + N - J + 1
        END DO
     ELSE
        JX = KX
        DO J = 1,N
           IF (X(JX) .NE. ZERO) THEN
              TEMP = ALPHA*CONJG(X(JX))
              AP(KK) = REAL(AP(KK)) + REAL(TEMP*X(JX))
              IX = JX
              DO K = KK + 1,KK + N - J
                 IX = IX + INCX
                 AP(K) = AP(K) + X(IX)*TEMP
              END DO
           ELSE
              AP(KK) = REAL(AP(KK))
           END IF
           JX = JX + INCX
           KK = KK + N - J + 1
        END DO
     END IF
  END IF
!
!     End of XHPR  .
!
END SUBROUTINE XHPR

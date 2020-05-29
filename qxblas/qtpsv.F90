!> \brief \b QTPSV
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       CHARACTER DIAG,TRANS,UPLO
!       ..
!       .. Array Arguments ..
!       REAL(WP) AP(*),X(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QTPSV  solves one of the systems of equations
!>
!>    A*x = b,   or   A**T*x = b,
!>
!> where b and x are n element vectors and A is an n by n unit, or
!> non-unit, upper or lower triangular matrix, supplied in packed form.
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
!>              TRANS = 'C' or 'c'   A**T*x = b.
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
!> \param[in] AP
!> \verbatim
!>          AP is REAL(WP) array of DIMENSION at least
!>           ( ( n*( n + 1 ) )/2 ).
!>           Before entry with  UPLO = 'U' or 'u', the array AP must
!>           contain the upper triangular matrix packed sequentially,
!>           column by column, so that AP( 1 ) contains a( 1, 1 ),
!>           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
!>           respectively, and so on.
!>           Before entry with UPLO = 'L' or 'l', the array AP must
!>           contain the lower triangular matrix packed sequentially,
!>           column by column, so that AP( 1 ) contains a( 1, 1 ),
!>           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
!>           respectively, and so on.
!>           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!>           A are not referenced, but are assumed to be unity.
!> \endverbatim
!>
!> \param[in,out] X
!> \verbatim
!>          X is REAL(WP) array of dimension at least
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
!> \ingroup single_blas_level2
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
SUBROUTINE QTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
!
!  -- Reference BLAS level2 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,N
  CHARACTER, INTENT(IN) :: DIAG,TRANS,UPLO
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: AP(*)
  REAL(WP), INTENT(INOUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: TEMP
  INTEGER :: I,INFO,IX,J,JX,K,KK,KX
  LOGICAL :: NOUNIT
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
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
  ELSE IF (INCX .EQ. 0) THEN
     INFO = 7
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('QTPSV ',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF (N .EQ. 0) RETURN
!
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
!     Start the operations. In this version the elements of AP are
!     accessed sequentially with one pass through AP.
!
  IF (LSAME(TRANS,'N')) THEN
!
!        Form  x := inv( A )*x.
!
     IF (LSAME(UPLO,'U')) THEN
        KK = (N * (N+1))/2
        IF (INCX .EQ. 1) THEN
           DO J = N,1,-1
              IF (X(J) .NE. ZERO) THEN
                 IF (NOUNIT) X(J) = X(J)/AP(KK)
                 TEMP = X(J)
                 K = KK - 1
                 DO I = J - 1,1,-1
                    X(I) = X(I) - TEMP*AP(K)
                    K = K - 1
                 END DO
              END IF
              KK = KK - J
           END DO
        ELSE
           JX = KX + (N-1)*INCX
           DO J = N,1,-1
              IF (X(JX) .NE. ZERO) THEN
                 IF (NOUNIT) X(JX) = X(JX)/AP(KK)
                 TEMP = X(JX)
                 IX = JX
                 DO K = KK - 1,KK - J + 1,-1
                    IX = IX - INCX
                    X(IX) = X(IX) - TEMP*AP(K)
                 END DO
              END IF
              JX = JX - INCX
              KK = KK - J
           END DO
        END IF
     ELSE
        KK = 1
        IF (INCX .EQ. 1) THEN
           DO J = 1,N
              IF (X(J) .NE. ZERO) THEN
                 IF (NOUNIT) X(J) = X(J)/AP(KK)
                 TEMP = X(J)
                 K = KK + 1
                 DO I = J + 1,N
                    X(I) = X(I) - TEMP*AP(K)
                    K = K + 1
                 END DO
              END IF
              KK = KK + (N-J+1)
           END DO
        ELSE
           JX = KX
           DO J = 1,N
              IF (X(JX) .NE. ZERO) THEN
                 IF (NOUNIT) X(JX) = X(JX)/AP(KK)
                 TEMP = X(JX)
                 IX = JX
                 DO K = KK + 1,KK + N - J
                    IX = IX + INCX
                    X(IX) = X(IX) - TEMP*AP(K)
                 END DO
              END IF
              JX = JX + INCX
              KK = KK + (N-J+1)
           END DO
        END IF
     END IF
  ELSE
!
!        Form  x := inv( A**T )*x.
!
     IF (LSAME(UPLO,'U')) THEN
        KK = 1
        IF (INCX .EQ. 1) THEN
           DO J = 1,N
              TEMP = X(J)
              K = KK
              DO I = 1,J - 1
                 TEMP = TEMP - AP(K)*X(I)
                 K = K + 1
              END DO
              IF (NOUNIT) TEMP = TEMP/AP(KK+J-1)
              X(J) = TEMP
              KK = KK + J
           END DO
        ELSE
           JX = KX
           DO J = 1,N
              TEMP = X(JX)
              IX = KX
              DO K = KK,KK + J - 2
                 TEMP = TEMP - AP(K)*X(IX)
                 IX = IX + INCX
              END DO
              IF (NOUNIT) TEMP = TEMP/AP(KK+J-1)
              X(JX) = TEMP
              JX = JX + INCX
              KK = KK + J
           END DO
        END IF
     ELSE
        KK = (N * (N+1))/2
        IF (INCX .EQ. 1) THEN
           DO J = N,1,-1
              TEMP = X(J)
              K = KK
              DO I = N,J + 1,-1
                 TEMP = TEMP - AP(K)*X(I)
                 K = K - 1
              END DO
              IF (NOUNIT) TEMP = TEMP/AP(KK-N+J)
              X(J) = TEMP
              KK = KK - (N-J+1)
           END DO
        ELSE
           KX = KX + (N-1)*INCX
           JX = KX
           DO J = N,1,-1
              TEMP = X(JX)
              IX = KX
              DO K = KK,KK - (N- (J+1)),-1
                 TEMP = TEMP - AP(K)*X(IX)
                 IX = IX - INCX
              END DO
              IF (NOUNIT) TEMP = TEMP/AP(KK-N+J)
              X(JX) = TEMP
              JX = JX - INCX
              KK = KK - (N-J+1)
           END DO
        END IF
     END IF
  END IF
!
!     End of QTPSV .
!
END SUBROUTINE QTPSV

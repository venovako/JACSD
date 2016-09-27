!> \brief \b DROTM
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE DROTM(N,DX,INCX,DY,INCY,DPARAM)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION DPARAM(5),DX(*),DY(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
!>
!>    (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
!>    (DY**T)
!>
!>    DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!>    LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
!>    WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!>
!>    DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
!>
!>      (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
!>    H=(          )    (          )    (          )    (          )
!>      (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
!>    SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in,out] DX
!> \verbatim
!>          DX is DOUBLE PRECISION array, dimension N
!>         double precision vector with N elements
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of DX
!> \endverbatim
!>
!> \param[in,out] DY
!> \verbatim
!>          DY is DOUBLE PRECISION array, dimension N
!>         double precision vector with N elements
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>         storage spacing between elements of DY
!> \endverbatim
!>
!> \param[in,out] DPARAM
!> \verbatim
!>          DPARAM is DOUBLE PRECISION array, dimension 5
!>     DPARAM(1)=DFLAG
!>     DPARAM(2)=DH11
!>     DPARAM(3)=DH21
!>     DPARAM(4)=DH12
!>     DPARAM(5)=DH22
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
!> \ingroup double_blas_level1
!
!  =====================================================================
PURE SUBROUTINE DROTM(N, DX, INCX, DY, INCY, DPARAM)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: N, INCX, INCY
!     ..
!     .. Array Arguments ..
  DOUBLE PRECISION, INTENT(INOUT) :: DX(*), DY(*)
  DOUBLE PRECISION, INTENT(IN) :: DPARAM(5)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  DOUBLE PRECISION :: DFLAG,DH11,DH12,DH21,DH22,W,Z
  INTEGER :: I,KX,KY,NSTEPS
!     ..
!
  DFLAG = DPARAM(1)
  IF ((N .LE. 0) .OR. (DFLAG .EQ. D_MTWO)) RETURN
  IF ((INCX .EQ. INCY) .AND. (INCX .GT. 0)) THEN
!
     NSTEPS = N * INCX
     IF (DFLAG .LT. D_ZERO) THEN
        DH11 = DPARAM(2)
        DH12 = DPARAM(4)
        DH21 = DPARAM(3)
        DH22 = DPARAM(5)
        DO I = 1,NSTEPS,INCX
           W = DX(I)
           Z = DY(I)
           DX(I) = W*DH11 + Z*DH12
           DY(I) = W*DH21 + Z*DH22
        END DO
     ELSE IF (DFLAG .EQ. D_ZERO) THEN
        DH12 = DPARAM(4)
        DH21 = DPARAM(3)
        DO I = 1,NSTEPS,INCX
           W = DX(I)
           Z = DY(I)
           DX(I) = W + Z*DH12
           DY(I) = W*DH21 + Z
        END DO
     ELSE
        DH11 = DPARAM(2)
        DH22 = DPARAM(5)
        DO I = 1,NSTEPS,INCX
           W = DX(I)
           Z = DY(I)
           DX(I) = W*DH11 + Z
           DY(I) = -W + DH22*Z
        END DO
     END IF
  ELSE
     KX = 1
     KY = 1
     IF (INCX .LT. 0) KX = 1 + (1-N)*INCX
     IF (INCY .LT. 0) KY = 1 + (1-N)*INCY
!
     IF (DFLAG .LT. D_ZERO) THEN
        DH11 = DPARAM(2)
        DH12 = DPARAM(4)
        DH21 = DPARAM(3)
        DH22 = DPARAM(5)
        DO I = 1,N
           W = DX(KX)
           Z = DY(KY)
           DX(KX) = W*DH11 + Z*DH12
           DY(KY) = W*DH21 + Z*DH22
           KX = KX + INCX
           KY = KY + INCY
        END DO
     ELSE IF (DFLAG .EQ. D_ZERO) THEN
        DH12 = DPARAM(4)
        DH21 = DPARAM(3)
        DO I = 1,N
           W = DX(KX)
           Z = DY(KY)
           DX(KX) = W + Z*DH12
           DY(KY) = W*DH21 + Z
           KX = KX + INCX
           KY = KY + INCY
        END DO
     ELSE
        DH11 = DPARAM(2)
        DH22 = DPARAM(5)
        DO I = 1,N
           W = DX(KX)
           Z = DY(KY)
           DX(KX) = W*DH11 + Z
           DY(KY) = -W + DH22*Z
           KX = KX + INCX
           KY = KY + INCY
        END DO
     END IF
  END IF
END SUBROUTINE DROTM

PURE SUBROUTINE SLACPY(UPLO, M, N, A, LDA, B, LDB)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA, LDB
  REAL, INTENT(IN) :: A(LDA,*)
  REAL, INTENT(OUT) :: B(LDB,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
     DO J = 1, N
        DO I = 1, MIN(J,M)
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
     DO J = 1, N
        DO I = J, M
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE
     DO J = 1, N
        DO I = 1, M
           B(I,J) = A(I,J)
        END DO
     END DO
  END IF

END SUBROUTINE SLACPY

PURE SUBROUTINE DLACPY(UPLO, M, N, A, LDA, B, LDB)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA, LDB
  DOUBLE PRECISION, INTENT(IN) :: A(LDA,*)
  DOUBLE PRECISION, INTENT(OUT) :: B(LDB,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
     DO J = 1, N
        DO I = 1, MIN(J,M)
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
     DO J = 1, N
        DO I = J, M
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE
     DO J = 1, N
        DO I = 1, M
           B(I,J) = A(I,J)
        END DO
     END DO
  END IF

END SUBROUTINE DLACPY

PURE SUBROUTINE CLACPY(UPLO, M, N, A, LDA, B, LDB)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA, LDB
  COMPLEX, INTENT(IN) :: A(LDA,*)
  COMPLEX, INTENT(OUT) :: B(LDB,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
     DO J = 1, N
        DO I = 1, MIN(J,M)
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
     DO J = 1, N
        DO I = J, M
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE
     DO J = 1, N
        DO I = 1, M
           B(I,J) = A(I,J)
        END DO
     END DO
  END IF

END SUBROUTINE CLACPY

PURE SUBROUTINE ZLACPY(UPLO, M, N, A, LDA, B, LDB)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA, LDB
  DOUBLE COMPLEX, INTENT(IN) :: A(LDA,*)
  DOUBLE COMPLEX, INTENT(OUT) :: B(LDB,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
     DO J = 1, N
        DO I = 1, MIN(J,M)
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
     DO J = 1, N
        DO I = J, M
           B(I,J) = A(I,J)
        END DO
     END DO
  ELSE
     DO J = 1, N
        DO I = 1, M
           B(I,J) = A(I,J)
        END DO
     END DO
  END IF

END SUBROUTINE ZLACPY

PURE SUBROUTINE SLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA
  REAL, INTENT(IN) :: ALPHA, BETA
  REAL, INTENT(OUT) :: A(LDA,*)
  
  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1, M)
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 1, MIN(M,N)
        DO I = J+1, M
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
  END IF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
  DO I = 1, MIN(M,N)
     A(I,I) = BETA
  END DO

END SUBROUTINE SLASET

PURE SUBROUTINE DLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA
  DOUBLE PRECISION, INTENT(IN) :: ALPHA, BETA
  DOUBLE PRECISION, INTENT(OUT) :: A(LDA,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1, M)
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 1, MIN(M,N)
        DO I = J+1, M
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
  END IF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
  DO I = 1, MIN(M,N)
     A(I,I) = BETA
  END DO

END SUBROUTINE DLASET

PURE SUBROUTINE CLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA
  COMPLEX, INTENT(IN) :: ALPHA, BETA
  COMPLEX, INTENT(OUT) :: A(LDA,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1, M)
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 1, MIN(M,N)
        DO I = J+1, M
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
  END IF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
  DO I = 1, MIN(M,N)
     A(I,I) = BETA
  END DO

END SUBROUTINE CLASET

PURE SUBROUTINE ZLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: M, N, LDA
  DOUBLE COMPLEX, INTENT(IN) :: ALPHA, BETA
  DOUBLE COMPLEX, INTENT(OUT) :: A(LDA,*)

  INTEGER :: I, J
  LOGICAL, EXTERNAL :: LSAME
  INTEGER, INTRINSIC :: MIN

  IF (LSAME(UPLO,'U')) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1, M)
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE IF (LSAME(UPLO,'L')) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 1, MIN(M,N)
        DO I = J+1, M
           A(I,J) = ALPHA
        END DO
     END DO
  ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
  END IF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
  DO I = 1, MIN(M,N)
     A(I,I) = BETA
  END DO

END SUBROUTINE ZLASET

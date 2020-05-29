!> \brief \b QNRM2
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QNRM2(N,X,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       REAL(WP) X(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QNRM2 returns the euclidean norm of a vector via the function
!> name, so that
!>
!>    QNRM2 := sqrt( x'*x ).
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
!> \ingroup single_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  -- This version written on 25-October-1982.
!>     Modified on 14-October-1993 to inline the call to SLASSQ.
!>     Sven Hammarling, Nag Ltd.
!> \endverbatim
!>
!  =====================================================================
FUNCTION QNRM2(N, X, INCX)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX, N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: X(*)
!     ..
!
  REAL(WP) :: QNRM2
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE=1.0E+0_WP, ZERO=0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: ABSXI, NORM, SCAL, SSQ
  INTEGER :: IX
!     ..
  IF ((N .LT. 1) .OR. (INCX .LT. 1)) THEN
     NORM = ZERO
  ELSE IF (N .EQ. 1) THEN
     NORM = ABS(X(1))
  ELSE
     SCAL = ZERO
     SSQ = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL SLASSQ( N, X, INCX, SCAL, SSQ )
!
     DO IX = 1, 1 + (N-1)*INCX, INCX
        IF (X(IX) .NE. ZERO) THEN
           ABSXI = ABS(X(IX))
           IF (SCAL .LT. ABSXI) THEN
              SSQ = ONE + SSQ * (SCAL/ABSXI)**2
              SCAL = ABSXI
           ELSE
              SSQ = SSQ + (ABSXI/SCAL)**2
           END IF
        END IF
     END DO
     NORM = SCAL * SQRT(SSQ)
  END IF
!
  QNRM2 = NORM
!
!     End of QNRM2.
!
END FUNCTION QNRM2

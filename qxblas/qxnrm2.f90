!> \brief \b QXNRM2
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       REAL(WP) FUNCTION QXNRM2(N,X,INCX)
! 
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) X(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QXNRM2 returns the euclidean norm of a vector via the function
!> name, so that
!>
!>    QXNRM2 := sqrt( x**H*x )
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
!>     Modified on 14-October-1993 to inline the call to XLASSQ.
!>     Sven Hammarling, Nag Ltd.
!> \endverbatim
!>
!  =====================================================================
FUNCTION QXNRM2(N,X,INCX)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(IN) :: X(*)
!     ..
!
  REAL(WP) :: QXNRM2
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  REAL(WP) :: NORM,SCAL,SSQ,TEMP
  INTEGER :: IX
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS,AIMAG,REAL,SQRT
!     ..
  IF ((N .LT. 1) .OR. (INCX .LT. 1)) THEN
     NORM = ZERO
  ELSE
     SCAL = ZERO
     SSQ = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL XLASSQ(N, X, INCX, SCAL, SSQ)
!
     DO IX = 1,1 + (N-1)*INCX,INCX
        IF (REAL(X(IX)) .NE. ZERO) THEN
           TEMP = ABS(REAL(X(IX)))
           IF (SCAL .LT. TEMP) THEN
              SSQ = ONE + SSQ * (SCAL/TEMP)**2
              SCAL = TEMP
           ELSE
              SSQ = SSQ + (TEMP/SCAL)**2
           END IF
        END IF
        IF (AIMAG(X(IX)) .NE. ZERO) THEN
           TEMP = ABS(AIMAG(X(IX)))
           IF (SCAL .LT. TEMP) THEN
              SSQ = ONE + SSQ * (SCAL/TEMP)**2
              SCAL = TEMP
           ELSE
              SSQ = SSQ + (TEMP/SCAL)**2
           END IF
        END IF
     END DO
     NORM = SCAL * SQRT(SSQ)
  END IF
!
  QXNRM2 = NORM
!
!     End of QXNRM2.
!
END FUNCTION QXNRM2

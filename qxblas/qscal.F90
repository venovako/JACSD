!> \brief \b QSCAL
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QSCAL(N,SA,SX,INCX)
! 
!       .. Scalar Arguments ..
!       REAL(WP) SA
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       REAL(WP) SX(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    scales a vector by a constant.
!>    uses unrolled loops for increment equal to 1.
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
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE QSCAL(N,SA,SX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: SA
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(INOUT) :: SX(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I,NINCX
!     ..
  IF ((N .LE. 0) .OR. (INCX .LE. 0)) RETURN
  IF (INCX .EQ. 1) THEN
!
!        code for increment equal to 1
!
     DO I = 1,N
        SX(I) = SA*SX(I)
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     NINCX = N*INCX
     DO I = 1,NINCX,INCX
        SX(I) = SA*SX(I)
     END DO
  END IF

END SUBROUTINE QSCAL

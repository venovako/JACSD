!> \brief \b XSCAL
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XSCAL(N,CA,CX,INCX)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) CA
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) CX(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    XSCAL scales a vector by a constant.
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
!> \ingroup complex_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack,  3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE XSCAL(N,CA,CX,INCX)
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
  COMPLEX(WP), INTENT(IN) :: CA
  INTEGER, INTENT(IN) :: INCX,N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(INOUT) :: CX(*)
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
        CX(I) = CA*CX(I)
     END DO
  ELSE
!
!        code for increment not equal to 1
!
     NINCX = N*INCX
     DO I = 1,NINCX,INCX
        CX(I) = CA*CX(I)
     END DO
  END IF
END SUBROUTINE XSCAL

!> \brief \b XLACGV conjugates a complex vector.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!
!  Definition:
!  ===========
!
!       SUBROUTINE XLACGV( N, X, INCX )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP)        X( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLACGV conjugates a complex vector of length N.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The length of the vector X.  N >= 0.
!> \endverbatim
!>
!> \param[in,out] X
!> \verbatim
!>          X is COMPLEX(WP) array, dimension
!>                         (1+(N-1)*abs(INCX))
!>          On entry, the vector of length N to be conjugated.
!>          On exit, X is overwritten with conjg(X).
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The spacing between successive elements of X.
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
!> \date December 2016
!
!> \ingroup complex16OTHERauxiliary
!
!  =====================================================================
SUBROUTINE XLACGV(N, X, INCX)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX, N
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(INOUT) :: X(*)
!     ..
!
! =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I, IOFF
!     ..
!     .. Executable Statements ..
!
  IF (INCX .EQ. 1) THEN
     DO I = 1, N
        X(I) = CONJG(X(I))
     END DO
  ELSE
     IOFF = 1
     IF (INCX .LT. 0) IOFF = 1 - (N-1)*INCX
     DO I = 1, N
        X(IOFF) = CONJG(X(IOFF))
        IOFF = IOFF + INCX
     END DO
  END IF
!
!     End of XLACGV
!
END SUBROUTINE XLACGV

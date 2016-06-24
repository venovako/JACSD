!> \brief \b QLASSQ updates a sum of squares represented in scaled form.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLASSQ + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlassq.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlassq.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlassq.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLASSQ( N, X, INCX, SCAL, SUMSQ )
! 
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       REAL(WP)           SCAL, SUMSQ
!       ..
!       .. Array Arguments ..
!       REAL(WP)           X( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLASSQ  returns the values  scl  and  smsq  such that
!>
!>    ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
!>
!> where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
!> assumed to be non-negative and  scl  returns the value
!>
!>    scl = max( scale, abs( x( i ) ) ).
!>
!> scale and sumsq must be supplied in SCAL and SUMSQ and
!> scl and smsq are overwritten on SCAL and SUMSQ respectively.
!>
!> The routine makes only one pass through the vector x.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements to be used from the vector X.
!> \endverbatim
!>
!> \param[in] X
!> \verbatim
!>          X is REAL(WP) array, dimension (N)
!>          The vector for which a scaled sum of squares is computed.
!>             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of the vector X.
!>          INCX > 0.
!> \endverbatim
!>
!> \param[in,out] SCAL
!> \verbatim
!>          SCAL is REAL(WP)
!>          On entry, the value  scale  in the equation above.
!>          On exit, SCAL is overwritten with  scl , the scaling factor
!>          for the sum of squares.
!> \endverbatim
!>
!> \param[in,out] SUMSQ
!> \verbatim
!>          SUMSQ is REAL(WP)
!>          On entry, the value  sumsq  in the equation above.
!>          On exit, SUMSQ is overwritten with  smsq , the basic sum of
!>          squares from which  scl  has been factored out.
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
!> \date September 2012
!
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
SUBROUTINE QLASSQ( N, X, INCX, SCAL, SUMSQ )
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX, N
  REAL(WP), INTENT(INOUT) :: SCAL, SUMSQ
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(IN) :: X( * )
!     ..
!
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: IX
  REAL(WP) :: ABSXI
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: QISNAN
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS
!     ..
!     .. Executable Statements ..
!
  IF (N .GT. 0) THEN
     DO IX = 1, 1 + (N-1)*INCX, INCX
        ABSXI = ABS(X(IX))
        IF ((ABSXI .GT. ZERO) .OR. QISNAN(ABSXI)) THEN
           IF (SCAL .LT. ABSXI) THEN
              SUMSQ = 1 + SUMSQ * (SCAL/ABSXI)**2
              SCAL = ABSXI
           ELSE
              SUMSQ = SUMSQ + (ABSXI/SCAL)**2
           END IF
        END IF
     END DO
  END IF
!
!     End of QLASSQ
!
END SUBROUTINE QLASSQ

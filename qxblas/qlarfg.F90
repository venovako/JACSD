!> \brief \b QLARFG generates an elementary reflector (Householder matrix).
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLARFG + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlarfg.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlarfg.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlarfg.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLARFG(N, ALPHA, X, INCX, TAU)
! 
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       REAL(WP)           ALPHA, TAU
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
!> QLARFG generates a real elementary reflector H of order n, such
!> that
!>
!>       H * ( alpha ) = ( beta ),   H**T * H = I.
!>           (   x   )   (   0  )
!>
!> where alpha and beta are scalars, and x is an (n-1)-element real
!> vector. H is represented in the form
!>
!>       H = I - tau * ( 1 ) * ( 1 v**T ) ,
!>                     ( v )
!>
!> where tau is a real scalar and v is a real (n-1)-element
!> vector.
!>
!> If the elements of x are all zero, then tau = 0 and H is taken to be
!> the unit matrix.
!>
!> Otherwise  1 <= tau <= 2.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The order of the elementary reflector.
!> \endverbatim
!>
!> \param[in,out] ALPHA
!> \verbatim
!>          ALPHA is REAL(WP)
!>          On entry, the value alpha.
!>          On exit, it is overwritten with the value beta.
!> \endverbatim
!>
!> \param[in,out] X
!> \verbatim
!>          X is REAL(WP) array, dimension
!>                         (1+(N-2)*abs(INCX))
!>          On entry, the vector x.
!>          On exit, it is overwritten with the vector v.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between elements of X. INCX > 0.
!> \endverbatim
!>
!> \param[out] TAU
!> \verbatim
!>          TAU is REAL(WP)
!>          The value tau.
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
!> \ingroup realOTHERauxiliary
!
!  =====================================================================
SUBROUTINE QLARFG(N, ALPHA, X, INCX, TAU)
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: INCX, N
  REAL(WP), INTENT(INOUT) :: ALPHA
  REAL(WP), INTENT(OUT) :: TAU
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(INOUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP, ZERO = 0.0E+0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: J, KNT
  REAL(WP) :: BETA, RSAFMN, SAFMIN, XNORM
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QLAMCH, QNRM2
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QSCAL
!     ..
!     .. Executable Statements ..
!
  IF (N .LE. 1) THEN
     TAU = ZERO
     RETURN
  END IF
!
  XNORM = QNRM2(N-1, X, INCX)
!
  IF (XNORM .EQ. ZERO) THEN
!
!        H  =  I
!
     TAU = ZERO
  ELSE
!
!        general case
!
     BETA = -SIGN(HYPOT(ALPHA,XNORM), ALPHA)
     SAFMIN = QLAMCH('S') / QLAMCH('E')
     KNT = 0
     IF (ABS(BETA) .LT. SAFMIN) THEN
!
!           XNORM, BETA may be inaccurate; scale X and recompute them
!
        RSAFMN = ONE / SAFMIN
1       CONTINUE
        KNT = KNT + 1
        CALL QSCAL(N-1, RSAFMN, X, INCX)
        BETA = BETA*RSAFMN
        ALPHA = ALPHA*RSAFMN
        IF (ABS(BETA) .LT. SAFMIN) GOTO 1
!
!           New BETA is at most 1, at least SAFMIN
!
        XNORM = QNRM2(N-1, X, INCX)
        BETA = -SIGN(HYPOT(ALPHA,XNORM), ALPHA)
     END IF
     TAU = (BETA-ALPHA) / BETA
     CALL QSCAL(N-1, ONE / (ALPHA-BETA), X, INCX)
!
!        If ALPHA is subnormal, it may lose relative accuracy
!
     DO J = 1, KNT
        BETA = BETA*SAFMIN
     END DO
     ALPHA = BETA
  END IF
!
!     End of QLARFG
!
END SUBROUTINE QLARFG

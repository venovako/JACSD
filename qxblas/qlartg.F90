!> \brief \b QLARTG generates a plane rotation with real cosine and real sine.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLARTG + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/slartg.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/slartg.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/slartg.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLARTG( F, G, CS, SN, R )
! 
!       .. Scalar Arguments ..
!       REAL(WP)           CS, F, G, R, SN
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLARTG generate a plane rotation so that
!>
!>    [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
!>    [ -SN  CS  ]     [ G ]     [ 0 ]
!>
!> This is a slower, more accurate version of the BLAS1 routine SROTG,
!> with the following other differences:
!>    F and G are unchanged on return.
!>    If G=0, then CS=1 and SN=0.
!>    If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
!>       floating point operations (saves work in SBDSQR when
!>       there are zeros on the diagonal).
!>
!> If F exceeds G in magnitude, CS will be positive.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] F
!> \verbatim
!>          F is REAL(WP)
!>          The first component of vector to be rotated.
!> \endverbatim
!>
!> \param[in] G
!> \verbatim
!>          G is REAL(WP)
!>          The second component of vector to be rotated.
!> \endverbatim
!>
!> \param[out] CS
!> \verbatim
!>          CS is REAL(WP)
!>          The cosine of the rotation.
!> \endverbatim
!>
!> \param[out] SN
!> \verbatim
!>          SN is REAL(WP)
!>          The sine of the rotation.
!> \endverbatim
!>
!> \param[out] R
!> \verbatim
!>          R is REAL(WP)
!>          The nonzero component of the rotated vector.
!>
!>  This version has a few statements commented out for thread safety
!>  (machine parameters are computed on each entry). 10 feb 03, SJH.
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
SUBROUTINE QLARTG(F, G, CS, SN, R)
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: WP = QX_WP
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: F, G
  REAL(WP), INTENT(OUT) :: CS, R, SN
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ZERO = 0.0E0_WP, ONE = 1.0E0_WP, TWO = 2.0E0_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: COUNT, I
  REAL(WP) :: EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCAL
!     ..
!     .. External Functions ..
  REAL(WP), EXTERNAL :: QLAMCH
!     ..
!     .. Executable Statements ..
!
  SAFMIN = QLAMCH('S')
  EPS = QLAMCH('E')
  SAFMN2 = QLAMCH('B')**INT(LOG(SAFMIN/EPS) / LOG(QLAMCH('B')) / TWO)
  SAFMX2 = ONE / SAFMN2
  IF (G .EQ. ZERO) THEN
     CS = ONE
     SN = ZERO
     R = F
  ELSE IF (F .EQ. ZERO) THEN
     CS = ZERO
     SN = ONE
     R = G
  ELSE
     F1 = F
     G1 = G
     SCAL = MAX(ABS(F1), ABS(G1))
     IF (SCAL .GE. SAFMX2) THEN
        COUNT = 0
1       CONTINUE
        COUNT = COUNT + 1
        F1 = F1*SAFMN2
        G1 = G1*SAFMN2
        SCAL = MAX(ABS(F1), ABS(G1))
        IF (SCAL .GE. SAFMX2) GOTO 1
        R = HYPOT(F1, G1)
        CS = F1 / R
        SN = G1 / R
        DO I = 1, COUNT
           R = R*SAFMX2
        END DO
     ELSE IF (SCAL .LE. SAFMN2) THEN
        COUNT = 0
2       CONTINUE
        COUNT = COUNT + 1
        F1 = F1*SAFMX2
        G1 = G1*SAFMX2
        SCAL = MAX(ABS(F1), ABS(G1))
        IF (SCAL .LE. SAFMN2) GOTO 2
        R = HYPOT(F1, G1)
        CS = F1 / R
        SN = G1 / R
        DO I = 1, COUNT
           R = R*SAFMN2
        END DO
     ELSE
        R = HYPOT(F1, G1)
        CS = F1 / R
        SN = G1 / R
     END IF
     IF ((ABS(F) .GT. ABS(G)) .AND. (CS .LT. ZERO)) THEN
        CS = -CS
        SN = -SN
        R = -R
     END IF
  END IF
!
!     End of QLARTG
!
END SUBROUTINE QLARTG

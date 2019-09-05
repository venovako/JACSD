!> \brief \b QLAEV2 computes the eigenvalues and eigenvectors of a 2-by-2 symmetric/Hermitian matrix.
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
!       SUBROUTINE QLAEV2( A, B, C, RT1, RT2, CS1, SN1 )
!
!       .. Scalar Arguments ..
!       REAL(WP) A, B, C, CS1, RT1, RT2, SN1
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
!>    [  A   B  ]
!>    [  B   C  ].
!> On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
!> eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
!> eigenvector for RT1, giving the decomposition
!>
!>    [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
!>    [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] A
!> \verbatim
!>          A is REAL(WP)
!>          The (1,1) element of the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[in] B
!> \verbatim
!>          B is REAL(WP)
!>          The (1,2) element and the conjugate of the (2,1) element of
!>          the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[in] C
!> \verbatim
!>          C is REAL(WP)
!>          The (2,2) element of the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[out] RT1
!> \verbatim
!>          RT1 is REAL(WP)
!>          The eigenvalue of larger absolute value.
!> \endverbatim
!>
!> \param[out] RT2
!> \verbatim
!>          RT2 is REAL(WP)
!>          The eigenvalue of smaller absolute value.
!> \endverbatim
!>
!> \param[out] CS1
!> \verbatim
!>          CS1 is REAL(WP)
!> \endverbatim
!>
!> \param[out] SN1
!> \verbatim
!>          SN1 is REAL(WP)
!>          The vector (CS1, SN1) is a unit right eigenvector for RT1.
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
!> \ingroup OTHERauxiliary
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  RT1 is accurate to a few ulps barring over/underflow.
!>
!>  RT2 may be inaccurate if there is massive cancellation in the
!>  determinant A*C-B*B; higher precision or correctly rounded or
!>  correctly truncated arithmetic would be needed to compute RT2
!>  accurately in all cases.
!>
!>  CS1 and SN1 are accurate to a few ulps barring over/underflow.
!>
!>  Overflow is possible only if RT1 is within a factor of 5 of overflow.
!>  Underflow is harmless if the input data is 0 or exceeds
!>     underflow_threshold / macheps.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE QLAEV2(A, B, C, RT1, RT2, CS1, SN1)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: A, B, C
  REAL(WP), INTENT(OUT) :: CS1, RT1, RT2, SN1
!     ..
!
! =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0_WP, TWO = 2.0_WP, ZERO = 0.0_WP, HALF = 0.5_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: SGN1, SGN2
  REAL(WP) :: AB, ACMN, ACMX, ACS, ADF, CS, CT, DF, RT, SM, TB, TN
!     ..
!     .. Executable Statements ..
!
!     Compute the eigenvalues
!
  SM = A + C
  DF = A - C
  ADF = ABS(DF)
  TB = B + B
  AB = ABS(TB)
  IF (ABS(A) .GT. ABS(C)) THEN
     ACMX = A
     ACMN = C
  ELSE
     ACMX = C
     ACMN = A
  END IF
  IF (ADF .GT. AB) THEN
     RT = ADF * SQRT(ONE + (AB / ADF)**2)
  ELSE IF (ADF .LT. AB) THEN
     RT = AB * SQRT(ONE + (ADF / AB)**2)
  ELSE
!
!        Includes case AB=ADF=0
!
     RT = AB * SQRT(TWO)
  END IF
  IF (SM .LT. ZERO) THEN
     RT1 = HALF * (SM-RT)
     SGN1 = -1
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
     RT2 = (ACMX / RT1) * ACMN - (B / RT1) * B
  ELSE IF (SM .GT. ZERO) THEN
     RT1 = HALF * (SM+RT)
     SGN1 = 1
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
     RT2 = (ACMX / RT1) * ACMN - (B / RT1) * B
  ELSE
!
!        Includes case RT1 = RT2 = 0
!
     RT1 = HALF * RT
     RT2 = -HALF * RT
     SGN1 = 1
  END IF
!
!     Compute the eigenvector
!
  IF (DF .GE. ZERO) THEN
     CS = DF + RT
     SGN2 = 1
  ELSE
     CS = DF - RT
     SGN2 = -1
  END IF
  ACS = ABS(CS)
  IF (ACS .GT. AB) THEN
     CT = -TB / CS
     SN1 = ONE / SQRT(ONE + CT * CT)
     CS1 = CT * SN1
  ELSE
     IF (AB .EQ. ZERO) THEN
        CS1 = ONE
        SN1 = ZERO
     ELSE
        TN = -CS / TB
        CS1 = ONE / SQRT(ONE + TN * TN)
        SN1 = TN * CS1
     END IF
  END IF
  IF (SGN1 .EQ. SGN2) THEN
     TN = CS1
     CS1 = -SN1
     SN1 = TN
  END IF
!
!     End of QLAEV2
!
END SUBROUTINE QLAEV2

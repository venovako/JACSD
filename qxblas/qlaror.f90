!> \brief \b QLAROR
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLAROR( SIDE, INIT, M, N, A, LDA, ISEED, X, INFO )
! 
!       .. Scalar Arguments ..
!       CHARACTER          INIT, SIDE
!       INTEGER            INFO, LDA, M, N
!       ..
!       .. Array Arguments ..
!       INTEGER            ISEED( 4 )
!       REAL(WP)           A( LDA, * ), X( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLAROR pre- or post-multiplies an M by N matrix A by a random
!> orthogonal matrix U, overwriting A.  A may optionally be initialized
!> to the identity matrix before multiplying by U.  U is generated using
!> the method of G.W. Stewart (SIAM J. Numer. Anal. 17, 1980, 403-409).
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] SIDE
!> \verbatim
!>          SIDE is CHARACTER*1
!>          Specifies whether A is multiplied on the left or right by U.
!>          = 'L':         Multiply A on the left (premultiply) by U
!>          = 'R':         Multiply A on the right (postmultiply) by U'
!>          = 'C' or 'T':  Multiply A on the left by U and the right
!>                          by U' (Here, U' means U-transpose.)
!> \endverbatim
!>
!> \param[in] INIT
!> \verbatim
!>          INIT is CHARACTER*1
!>          Specifies whether or not A should be initialized to the
!>          identity matrix.
!>          = 'I':  Initialize A to (a section of) the identity matrix
!>                   before applying U.
!>          = 'N':  No initialization.  Apply U to the input matrix A.
!>
!>          INIT = 'I' may be used to generate square or rectangular
!>          orthogonal matrices:
!>
!>          For M = N and SIDE = 'L' or 'R', the rows will be orthogonal
!>          to each other, as will the columns.
!>
!>          If M < N, SIDE = 'R' produces a dense matrix whose rows are
!>          orthogonal and whose columns are not, while SIDE = 'L'
!>          produces a matrix whose rows are orthogonal, and whose first
!>          M columns are orthogonal, and whose remaining columns are
!>          zero.
!>
!>          If M > N, SIDE = 'L' produces a dense matrix whose columns
!>          are orthogonal and whose rows are not, while SIDE = 'R'
!>          produces a matrix whose columns are orthogonal, and whose
!>          first M rows are orthogonal, and whose remaining rows are
!>          zero.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of A.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of A.
!> \endverbatim
!>
!> \param[in,out] A
!> \verbatim
!>          A is REAL(WP) array, dimension (LDA, N)
!>          On entry, the array A.
!>          On exit, overwritten by U A ( if SIDE = 'L' ),
!>           or by A U ( if SIDE = 'R' ),
!>           or by U A U' ( if SIDE = 'C' or 'T').
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
!> \endverbatim
!>
!> \param[in,out] ISEED
!> \verbatim
!>          ISEED is INTEGER array, dimension (4)
!>          On entry ISEED specifies the seed of the random number
!>          generator. The array elements should be between 0 and 4095;
!>          if not they will be reduced mod 4096.  Also, ISEED(4) must
!>          be odd.  The random number generator uses a linear
!>          congruential sequence limited to small integers, and so
!>          should produce machine independent random numbers. The
!>          values of ISEED are changed on exit, and can be used in the
!>          next call to QLAROR to continue the same random number
!>          sequence.
!> \endverbatim
!>
!> \param[out] X
!> \verbatim
!>          X is REAL(WP) array, dimension (3*MAX( M, N ))
!>          Workspace of length
!>              2*M + N if SIDE = 'L',
!>              2*N + M if SIDE = 'R',
!>              3*N     if SIDE = 'C' or 'T'.
!> \endverbatim
!>
!> \param[out] INFO
!> \verbatim
!>          INFO is INTEGER
!>          An error flag.  It is set to:
!>          = 0:  normal return
!>          < 0:  if INFO = -k, the k-th argument had an illegal value
!>          = 1:  if the random numbers generated by SLARND are bad.
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
!> \ingroup real_matgen
!
!  =====================================================================
SUBROUTINE QLAROR(SIDE, INIT, M, N, A, LDA, ISEED, X, INFO)
!
!  -- LAPACK auxiliary routine (version 3.4.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: INIT, SIDE
  INTEGER, INTENT(IN) :: LDA, M, N
  INTEGER, INTENT(OUT) :: INFO
!     ..
!     .. Array Arguments ..
  INTEGER, INTENT(INOUT) :: ISEED(4)
  REAL(WP), INTENT(INOUT) :: A(LDA,*)
  REAL(WP), INTENT(OUT) :: X(*)
!     ..
!
!  =====================================================================
!
!     .. Parameters ..
  REAL(WP), PARAMETER :: ZERO = 0.0E+0_WP, ONE = 1.0E+0, TOOSML = 1.0E-20_WP
!     ..
!     .. Local Scalars ..
  INTEGER :: IROW, ITYPE, IXFRM, J, JCOL, KBEG, NXFRM
  REAL(WP) :: FACTOR, XNORM, XNORMS
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
  REAL(WP), EXTERNAL :: QLARND, QNRM2
!     ..
!     .. External Subroutines ..
  EXTERNAL :: QGEMV, QGER, QLASET, QSCAL, XERBLA
!     ..
!     .. Intrinsic Functions ..
  REAL(WP), INTRINSIC :: ABS, SIGN
!     ..
!     .. Executable Statements ..
!
  INFO = 0
  IF ((N .EQ. 0) .OR. (M .EQ. 0)) RETURN
!
  ITYPE = 0
  IF (LSAME(SIDE,'L')) THEN
     ITYPE = 1
  ELSE IF (LSAME(SIDE,'R')) THEN
     ITYPE = 2
  ELSE IF (LSAME(SIDE,'C') .OR. LSAME(SIDE,'T')) THEN
     ITYPE = 3
  END IF
!
!     Check for argument errors.
!
  IF (ITYPE .EQ. 0) THEN
     INFO = -1
  ELSE IF (M .LT. 0) THEN
     INFO = -3
  ELSE IF ((N .LT. 0) .OR. ((ITYPE .EQ. 3) .AND. (N .NE. M))) THEN
     INFO = -4
  ELSE IF (LDA .LT. M) THEN
     INFO = -6
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('QLAROR', -INFO)
     RETURN
  END IF
!
  IF (ITYPE .EQ. 1) THEN
     NXFRM = M
  ELSE
     NXFRM = N
  END IF
!
!     Initialize A to the identity matrix if desired
!
  IF (LSAME(INIT,'I')) CALL QLASET('Full', M, N, ZERO, ONE, A, LDA)
!
!     If no rotation possible, multiply by random +/-1
!
!     Compute rotation by computing Householder transformations
!     H(2), H(3), ..., H(nhouse)
!
  DO J = 1, NXFRM
     X(J) = ZERO
  END DO
!
  DO IXFRM = 2, NXFRM
     KBEG = NXFRM - IXFRM + 1
!
!        Generate independent normal( 0, 1 ) random numbers
!
     DO J = KBEG, NXFRM
        X(J) = QLARND(3,ISEED)
     END DO
!
!        Generate a Householder transformation from the random vector X
!
     XNORM = QNRM2(IXFRM, X(KBEG), 1)
     XNORMS = SIGN(XNORM, X(KBEG))
     X(KBEG+NXFRM) = SIGN(ONE, -X(KBEG))
     FACTOR = XNORMS*(XNORMS+X(KBEG))
     IF (ABS(FACTOR) .LT. TOOSML) THEN
        INFO = 1
        CALL XERBLA('QLAROR', INFO)
        RETURN
     ELSE
        FACTOR = ONE / FACTOR
     END IF
     X(KBEG) = X(KBEG) + XNORMS
!
!        Apply Householder transformation to A
!
     IF ((ITYPE .EQ. 1) .OR. (ITYPE .EQ. 3)) THEN
!
!           Apply H(k) from the left.
!
        CALL QGEMV('T', IXFRM, N, ONE, A(KBEG,1), LDA, X(KBEG), 1, ZERO, X(2*NXFRM+1), 1)
        CALL QGER(IXFRM, N, -FACTOR, X(KBEG), 1, X(2*NXFRM+1), 1, A(KBEG,1), LDA)
!
     END IF
!
     IF ((ITYPE .EQ. 2) .OR. (ITYPE .EQ. 3)) THEN
!
!           Apply H(k) from the right.
!
        CALL QGEMV('N', M, IXFRM, ONE, A(1,KBEG), LDA, X(KBEG), 1, ZERO, X(2*NXFRM+1), 1)
        CALL QGER(M, IXFRM, -FACTOR, X(2*NXFRM+1), 1, X(KBEG), 1, A(1,KBEG), LDA)
!
     END IF
  END DO
!
  X(2*NXFRM) = SIGN(ONE,QLARND(3,ISEED))
!
!     Scale the matrix A by D.
!
  IF ((ITYPE .EQ. 1) .OR. (ITYPE .EQ. 3)) THEN
     DO IROW = 1, M
        CALL QSCAL(N, X(NXFRM+IROW), A(IROW,1), LDA)
     END DO
  END IF
!
  IF ((ITYPE .EQ. 2) .OR. (ITYPE .EQ. 3)) THEN
     DO JCOL = 1, N
        CALL QSCAL(M, X(NXFRM+JCOL), A(1,JCOL), 1)
     END DO
  END IF
!
!     End of QLAROR
!
END SUBROUTINE QLAROR

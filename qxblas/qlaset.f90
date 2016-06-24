!> \brief \b QLASET initializes the off-diagonal elements and the diagonal elements of a matrix to given values.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QLASET + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qlaset.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qlaset.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qlaset.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       SUBROUTINE QLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
! 
!       .. Scalar Arguments ..
!       CHARACTER          UPLO
!       INTEGER            LDA, M, N
!       REAL(WP)           ALPHA, BETA
!       ..
!       .. Array Arguments ..
!       REAL(WP)           A( LDA, * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QLASET initializes an m-by-n matrix A to BETA on the diagonal and
!> ALPHA on the offdiagonals.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>          Specifies the part of the matrix A to be set.
!>          = 'U':      Upper triangular part is set; the strictly lower
!>                      triangular part of A is not changed.
!>          = 'L':      Lower triangular part is set; the strictly upper
!>                      triangular part of A is not changed.
!>          Otherwise:  All of the matrix A is set.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.  M >= 0.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.  N >= 0.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is REAL(WP)
!>          The constant to which the offdiagonal elements are to be set.
!> \endverbatim
!>
!> \param[in] BETA
!> \verbatim
!>          BETA is REAL(WP)
!>          The constant to which the diagonal elements are to be set.
!> \endverbatim
!>
!> \param[out] A
!> \verbatim
!>          A is REAL(WP) array, dimension (LDA,N)
!>          On exit, the leading m-by-n submatrix of A is set as follows:
!>
!>          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
!>          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
!>          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
!>
!>          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,M).
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
!> \date November 2015
!
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
SUBROUTINE QLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
!
!  -- LAPACK auxiliary routine (version 3.6.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2015
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, M, N
  REAL(WP), INTENT(IN) :: ALPHA, BETA
!     ..
!     .. Array Arguments ..
  REAL(WP), INTENT(OUT) :: A(LDA,*)
!     ..
!
! =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I, J
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. Intrinsic Functions ..
  INTEGER, INTRINSIC :: MIN
!     ..
!     .. Executable Statements ..
!
  IF (LSAME(UPLO, 'U')) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1,M)
           A(I,J) = ALPHA
        END DO
     END DO
!
  ELSE IF (LSAME(UPLO,'L')) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
     DO J = 1, MIN(M,N)
        DO I = J + 1, M
           A(I,J) = ALPHA
        END DO
     END DO
!
  ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
  END IF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
  DO I = 1, MIN(M,N)
     A(I,I) = BETA
  END DO
!
!     End of QLASET
!
END SUBROUTINE QLASET

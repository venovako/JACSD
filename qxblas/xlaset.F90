!> \brief \b XLASET initializes the off-diagonal elements and the diagonal elements of a matrix to given values.
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
!       SUBROUTINE XLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
!
!       .. Scalar Arguments ..
!       CHARACTER          UPLO
!       INTEGER            LDA, M, N
!       COMPLEX(WP)        ALPHA, BETA
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP)        A( LDA, * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLASET initializes a 2-D array A to BETA on the diagonal and
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
!>          = 'U':      Upper triangular part is set. The lower triangle
!>                      is unchanged.
!>          = 'L':      Lower triangular part is set. The upper triangle
!>                      is unchanged.
!>          Otherwise:  All of the matrix A is set.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>          On entry, M specifies the number of rows of A.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          On entry, N specifies the number of columns of A.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX(WP)
!>          All the offdiagonal array elements are set to ALPHA.
!> \endverbatim
!>
!> \param[in] BETA
!> \verbatim
!>          BETA is COMPLEX(WP)
!>          All the diagonal array elements are set to BETA.
!> \endverbatim
!>
!> \param[out] A
!> \verbatim
!>          A is COMPLEX(WP) array, dimension (LDA,N)
!>          On entry, the m by n matrix A.
!>          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;
!>                   A(i,i) = BETA , 1 <= i <= min(m,n)
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
!> \date December 2016
!
!> \ingroup complex16OTHERauxiliary
!
!  =====================================================================
SUBROUTINE XLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, M, N
  COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(OUT) :: A(LDA,*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
  INTEGER :: I, J
!     ..
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. Executable Statements ..
!
  IF (LSAME(UPLO, 'U')) THEN
!
!        Set the diagonal to BETA and the strictly upper triangular
!        part of the array to ALPHA.
!
     DO J = 2, N
        DO I = 1, MIN(J-1, M)
           A(I,J) = ALPHA
        END DO
     END DO
     DO I = 1, MIN(N,M)
        A(I,I) = BETA
     END DO
!
  ELSE IF (LSAME(UPLO, 'L')) THEN
!
!        Set the diagonal to BETA and the strictly lower triangular
!        part of the array to ALPHA.
!
     DO J = 1, MIN(M, N)
        DO I = J+1, M
           A(I,J) = ALPHA
        END DO
     END DO
     DO I = 1, MIN(N, M)
        A(I,I) = BETA
     END DO
!
  ELSE
!
!        Set the array to BETA on the diagonal and ALPHA on the
!        offdiagonal.
!
     DO J = 1, N
        DO I = 1, M
           A(I,J) = ALPHA
        END DO
     END DO
     DO I = 1, MIN(M, N)
        A(I,I) = BETA
     END DO
  END IF
!
!     End of XLASET
!
END SUBROUTINE XLASET

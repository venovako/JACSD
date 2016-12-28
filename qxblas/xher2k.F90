!> \brief \b XHER2K
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       SUBROUTINE XHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
! 
!       .. Scalar Arguments ..
!       COMPLEX(WP) ALPHA
!       REAL(WP) BETA
!       INTEGER K,LDA,LDB,LDC,N
!       CHARACTER TRANS,UPLO
!       ..
!       .. Array Arguments ..
!       COMPLEX(WP) A(LDA,*),B(LDB,*),C(LDC,*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XHER2K  performs one of the hermitian rank 2k operations
!>
!>    C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
!>
!> or
!>
!>    C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
!>
!> where  alpha and beta  are scalars with  beta  real,  C is an  n by n
!> hermitian matrix and  A and B  are  n by k matrices in the first case
!> and  k by n  matrices in the second case.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] UPLO
!> \verbatim
!>          UPLO is CHARACTER*1
!>           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!>           triangular  part  of the  array  C  is to be  referenced  as
!>           follows:
!>
!>              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!>                                  is to be referenced.
!>
!>              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!>                                  is to be referenced.
!> \endverbatim
!>
!> \param[in] TRANS
!> \verbatim
!>          TRANS is CHARACTER*1
!>           On entry,  TRANS  specifies the operation to be performed as
!>           follows:
!>
!>              TRANS = 'N' or 'n'    C := alpha*A*B**H          +
!>                                         conjg( alpha )*B*A**H +
!>                                         beta*C.
!>
!>              TRANS = 'C' or 'c'    C := alpha*A**H*B          +
!>                                         conjg( alpha )*B**H*A +
!>                                         beta*C.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry,  N specifies the order of the matrix C.  N must be
!>           at least zero.
!> \endverbatim
!>
!> \param[in] K
!> \verbatim
!>          K is INTEGER
!>           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!>           of  columns  of the  matrices  A and B,  and on  entry  with
!>           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
!>           matrices  A and B.  K must be at least zero.
!> \endverbatim
!>
!> \param[in] ALPHA
!> \verbatim
!>          ALPHA is COMPLEX(WP)
!>           On entry, ALPHA specifies the scalar alpha.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is COMPLEX(WP) array of DIMENSION ( LDA, ka ), where ka is
!>           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!>           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!>           part of the array  A  must contain the matrix  A,  otherwise
!>           the leading  k by n  part of the array  A  must contain  the
!>           matrix A.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>           On entry, LDA specifies the first dimension of A as declared
!>           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!>           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!>           be at least  max( 1, k ).
!> \endverbatim
!>
!> \param[in] B
!> \verbatim
!>          B is COMPLEX(WP) array of DIMENSION ( LDB, kb ), where kb is
!>           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!>           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!>           part of the array  B  must contain the matrix  B,  otherwise
!>           the leading  k by n  part of the array  B  must contain  the
!>           matrix B.
!> \endverbatim
!>
!> \param[in] LDB
!> \verbatim
!>          LDB is INTEGER
!>           On entry, LDB specifies the first dimension of B as declared
!>           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!>           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!>           be at least  max( 1, k ).
!> \endverbatim
!>
!> \param[in] BETA
!> \verbatim
!>          BETA is REAL(WP)
!>           On entry, BETA specifies the scalar beta.
!> \endverbatim
!>
!> \param[in,out] C
!> \verbatim
!>          C is COMPLEX(WP) array of DIMENSION ( LDC, n ).
!>           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!>           upper triangular part of the array C must contain the upper
!>           triangular part  of the  hermitian matrix  and the strictly
!>           lower triangular part of C is not referenced.  On exit, the
!>           upper triangular part of the array  C is overwritten by the
!>           upper triangular part of the updated matrix.
!>           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!>           lower triangular part of the array C must contain the lower
!>           triangular part  of the  hermitian matrix  and the strictly
!>           upper triangular part of C is not referenced.  On exit, the
!>           lower triangular part of the array  C is overwritten by the
!>           lower triangular part of the updated matrix.
!>           Note that the imaginary parts of the diagonal elements need
!>           not be set,  they are assumed to be zero,  and on exit they
!>           are set to zero.
!> \endverbatim
!>
!> \param[in] LDC
!> \verbatim
!>          LDC is INTEGER
!>           On entry, LDC specifies the first dimension of C as declared
!>           in  the  calling  (sub)  program.   LDC  must  be  at  least
!>           max( 1, n ).
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
!> \ingroup complex_blas_level3
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Level 3 Blas routine.
!>
!>  -- Written on 8-February-1989.
!>     Jack Dongarra, Argonne National Laboratory.
!>     Iain Duff, AERE Harwell.
!>     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!>     Sven Hammarling, Numerical Algorithms Group Ltd.
!>
!>  -- Modified 8-Nov-93 to set C(J,J) to REAL( C(J,J) ) when BETA = 1.
!>     Ed Anderson, Cray Research Inc.
!> \endverbatim
!>
!  =====================================================================
SUBROUTINE XHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
!
!  -- Reference BLAS level3 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!
!     .. Scalar Arguments ..
  COMPLEX(WP), INTENT(IN) :: ALPHA
  REAL(WP), INTENT(IN) :: BETA
  INTEGER, INTENT(IN) :: K,LDA,LDB,LDC,N
  CHARACTER, INTENT(IN) :: TRANS,UPLO
!     ..
!     .. Array Arguments ..
  COMPLEX(WP), INTENT(IN) :: A(LDA,*),B(LDB,*)
  COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
!     ..
!
!  =====================================================================
!
!     .. External Functions ..
  LOGICAL, EXTERNAL :: LSAME
!     ..
!     .. External Subroutines ..
  EXTERNAL :: XERBLA
!     ..
!     .. Local Scalars ..
  COMPLEX(WP) :: TEMP1,TEMP2
  INTEGER :: I,INFO,J,L,NROWA
  LOGICAL :: UPPER
!     ..
!     .. Parameters ..
  REAL(WP), PARAMETER :: ONE = 1.0E+0_WP
  REAL(WP), PARAMETER :: RZERO = 0.0E+0_WP
  COMPLEX(WP), PARAMETER :: ZERO = (0.0E+0_WP,0.0E+0_WP)
!     ..
!
!     Test the input parameters.
!
  IF (LSAME(TRANS,'N')) THEN
     NROWA = N
  ELSE
     NROWA = K
  END IF
  UPPER = LSAME(UPLO,'U')
!
  INFO = 0
  IF ((.NOT. UPPER) .AND. (.NOT. LSAME(UPLO,'L'))) THEN
     INFO = 1
  ELSE IF ((.NOT. LSAME(TRANS,'N')) .AND. (.NOT. LSAME(TRANS,'C'))) THEN
     INFO = 2
  ELSE IF (N .LT. 0) THEN
     INFO = 3
  ELSE IF (K .LT. 0) THEN
     INFO = 4
  ELSE IF (LDA .LT. MAX(1,NROWA)) THEN
     INFO = 7
  ELSE IF (LDB .LT. MAX(1,NROWA)) THEN
     INFO = 9
  ELSE IF (LDC .LT. MAX(1,N)) THEN
     INFO = 12
  END IF
  IF (INFO .NE. 0) THEN
     CALL XERBLA('XHER2K',INFO)
     RETURN
  END IF
!
!     Quick return if possible.
!
  IF ((N .EQ. 0) .OR. (((ALPHA .EQ. ZERO) .OR. (K .EQ. 0)) .AND. (BETA .EQ. ONE))) RETURN
!
!     And when  alpha.eq.zero.
!
  IF (ALPHA .EQ. ZERO) THEN
     IF (UPPER) THEN
        IF (BETA .EQ. RZERO) THEN
           DO J = 1,N
              DO I = 1,J
                 C(I,J) = ZERO
              END DO
           END DO
        ELSE
           DO J = 1,N
              DO I = 1,J - 1
                 C(I,J) = BETA*C(I,J)
              END DO
              C(J,J) = BETA*REAL(C(J,J))
           END DO
        END IF
     ELSE
        IF (BETA .EQ. RZERO) THEN
           DO J = 1,N
              DO I = J,N
                 C(I,J) = ZERO
              END DO
           END DO
        ELSE
           DO J = 1,N
              C(J,J) = BETA*REAL(C(J,J))
              DO I = J + 1,N
                 C(I,J) = BETA*C(I,J)
              END DO
           END DO
        END IF
     END IF
     RETURN
  END IF
!
!     Start the operations.
!
  IF (LSAME(TRANS,'N')) THEN
!
!        Form  C := alpha*A*B**H + conjg( alpha )*B*A**H + C.
!
     IF (UPPER) THEN
        DO J = 1,N
           IF (BETA .EQ. RZERO) THEN
              DO I = 1,J
                 C(I,J) = ZERO
              END DO
           ELSE IF (BETA .NE. ONE) THEN
              DO I = 1,J - 1
                 C(I,J) = BETA*C(I,J)
              END DO
              C(J,J) = BETA*REAL(C(J,J))
           ELSE
              C(J,J) = REAL(C(J,J))
           END IF
           DO L = 1,K
              IF ((A(J,L) .NE. ZERO) .OR. (B(J,L) .NE. ZERO)) THEN
                 TEMP1 = ALPHA*CONJG(B(J,L))
                 TEMP2 = CONJG(ALPHA*A(J,L))
                 DO I = 1,J - 1
                    C(I,J) = C(I,J) + A(I,L)*TEMP1 + B(I,L)*TEMP2
                 END DO
                 C(J,J) = REAL(C(J,J)) + REAL(A(J,L)*TEMP1+B(J,L)*TEMP2)
              END IF
           END DO
        END DO
     ELSE
        DO J = 1,N
           IF (BETA .EQ. RZERO) THEN
              DO I = J,N
                 C(I,J) = ZERO
              END DO
           ELSE IF (BETA .NE. ONE) THEN
              DO I = J + 1,N
                 C(I,J) = BETA*C(I,J)
              END DO
              C(J,J) = BETA*REAL(C(J,J))
           ELSE
              C(J,J) = REAL(C(J,J))
           END IF
           DO L = 1,K
              IF ((A(J,L) .NE. ZERO) .OR. (B(J,L) .NE. ZERO)) THEN
                 TEMP1 = ALPHA*CONJG(B(J,L))
                 TEMP2 = CONJG(ALPHA*A(J,L))
                 DO I = J + 1,N
                    C(I,J) = C(I,J) + A(I,L)*TEMP1 + B(I,L)*TEMP2
                 END DO
                 C(J,J) = REAL(C(J,J)) + REAL(A(J,L)*TEMP1+B(J,L)*TEMP2)
              END IF
           END DO
        END DO
     END IF
  ELSE
!
!        Form  C := alpha*A**H*B + conjg( alpha )*B**H*A + C.
!
     IF (UPPER) THEN
        DO J = 1,N
           DO I = 1,J
              TEMP1 = ZERO
              TEMP2 = ZERO
              DO L = 1,K
                 TEMP1 = TEMP1 + CONJG(A(L,I))*B(L,J)
                 TEMP2 = TEMP2 + CONJG(B(L,I))*A(L,J)
              END DO
              IF (I .EQ. J) THEN
                 IF (BETA .EQ. RZERO) THEN
                    C(J,J) = REAL(ALPHA*TEMP1+CONJG(ALPHA)*TEMP2)
                 ELSE
                    C(J,J) = BETA*REAL(C(J,J)) + REAL(ALPHA*TEMP1+CONJG(ALPHA)*TEMP2)
                 END IF
              ELSE
                 IF (BETA .EQ. RZERO) THEN
                    C(I,J) = ALPHA*TEMP1 + CONJG(ALPHA)*TEMP2
                 ELSE
                    C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + CONJG(ALPHA)*TEMP2
                 END IF
              END IF
           END DO
        END DO
     ELSE
        DO J = 1,N
           DO I = J,N
              TEMP1 = ZERO
              TEMP2 = ZERO
              DO L = 1,K
                 TEMP1 = TEMP1 + CONJG(A(L,I))*B(L,J)
                 TEMP2 = TEMP2 + CONJG(B(L,I))*A(L,J)
              END DO
              IF (I .EQ. J) THEN
                 IF (BETA .EQ. RZERO) THEN
                    C(J,J) = REAL(ALPHA*TEMP1+CONJG(ALPHA)*TEMP2)
                 ELSE
                    C(J,J) = BETA*REAL(C(J,J)) + REAL(ALPHA*TEMP1+CONJG(ALPHA)*TEMP2)
                 END IF
              ELSE
                 IF (BETA .EQ. RZERO) THEN
                    C(I,J) = ALPHA*TEMP1 + CONJG(ALPHA)*TEMP2
                 ELSE
                    C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + CONJG(ALPHA)*TEMP2
                 END IF
              END IF
           END DO
        END DO
     END IF
  END IF
!
!     End of XHER2K.
!
END SUBROUTINE XHER2K

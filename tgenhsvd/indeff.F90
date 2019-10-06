MODULE INDEFF
  IMPLICIT NONE
#include "qx_wp.fi"

  PRIVATE :: WP
  CHARACTER, PARAMETER, PRIVATE :: UPLO = 'L', SIDE = 'R'
  REAL(KIND=WP), PARAMETER, PRIVATE :: Q_ZERO = 0.0_WP, Q_ONE = 1.0_WP, Q_MONE = -1.0_WP
  REAL(KIND=WP), PARAMETER, PRIVATE :: Q_ALPHA = (Q_ONE + SQRT(17.0_WP)) / 8.0_WP

CONTAINS

  ! Symmetric Indefinite Factorization

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE QSYJF2(N, A, LDA, JVEC, IPIV, INFO)
    !     Modified by Singers, January 8, 2006.
    !
    !     Proposal by Ivan Slapnicar
    !     University of Split, Croatia
    !     slap@split.fesb.hr
    !     December 15, 1993
    !
    !     Purpose
    !     =======
    !
    !     QSYJF2 computes the factorization of a symmetric matrix A using
    !     a modification of the Bunch-Kaufman diagonal pivoting method:
    !
    !     A = L*J*L^T
    !
    !     where L is a product of permutation and lower block triangular
    !     matrices with 1-by-1 and 2-by-2 diagonal blocks, and J is diagonal,
    !     with diagonal elements equal to 1 or -1
    !     (see below for further details).
    !
    !     This is the unblocked version of the algorithm, calling Level 2 BLAS.
    !
    !     Arguments
    !     =========
    !
    !     N       (input) INTEGER
    !     The order of the matrix A.  N >= 0.
    !
    !     A       (input/output) REAL(KIND=WP) array, dimension (LDA,N)
    !     On entry, the symmetric matrix A.  The leading N-by-N lower
    !     triangular part of A contains the lower triangular part of the
    !     matrix A.  The strict upper triangular part will be destroyed!
    !
    !     On exit, the multipliers used to obtain the block triangular
    !     factor L (see below for further details).
    !
    !     LDA     (input) INTEGER
    !     The leading dimension of the array A.  LDA >= max(1,N).
    !
    !     JVEC    (output) INTEGER array, dimension(N)
    !     The diagonal of the matrix J.
    !
    !     IPIV    (output) INTEGER array, dimension (N)
    !     Details of the interchanges and the block structure of D.
    !     If IPIV(k) > 0, then rows and columns k and IPIV(k) were
    !     interchanged and in the k-th step a 1-by-1 diagonal block
    !     was used (see below for further details).
    !     If IPIV(k) < 0 and IPIV(k+1) < 0, then rows and columns k and
    !     -IPIV(k) and k+1 and -IPIV(k+1) were interchanged, and in the
    !     k-th step a 2-by-2 diagonal block was used.
    !
    !     INFO    (output) INTEGER
    !     = 0: successful exit
    !     < 0: if INFO = -k, the k-th argument had an illegal value
    !     If INFO = k, then in the k-th step the trailing submatrix
    !     A(k:n,k:n) was exactly zero, and the rank of A equals k-1.
    !
    !     Further Details
    !     ===============
    !
    !     A = L*J*L^T, where L = P(1)*L(1)* ... *P(k)*L(k)* ...,
    !     i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
    !     n in steps of 1 or 2, and J is a diagonal matrix with diagonal
    !     elements equal to 1 or -1.  P(k) is a permutation matrix as defined
    !     by IPIV(k), and L(k) is a lower block triangular matrix, such that
    !
    !             (   I    0     0   )  k-1
    !     L(k) =  (   0    d     0   )  s
    !             (   0    v     I   )  n-k-s+1
    !                k-1   s  n-k-s+1
    !
    !     Here s = 1 or 2, and ( d ) overwrites A(k:n,k:k+s-1).
    !                          ( v )
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    REAL(KIND=WP), INTENT(INOUT) :: A(LDA,N)
    INTEGER, INTENT(OUT) :: JVEC(N), IPIV(N), INFO

    INTEGER :: I, J, IMAX, JMAX, K, KDIAG, KP, KSTEP
    REAL(KIND=WP) :: DIAMAX, OFFMAX, R1, R2, C, S, T, TEMP

    INTEGER, EXTERNAL :: IQAMAX
    EXTERNAL :: QLAEV2, QROT, QSCAL, QSWAP, QSYR

    INFO = 0
    IF (LDA .LT. MAX(1,N)) INFO = -3
    IF (N .LT. 0) INFO = -1
    IF (INFO .NE. 0) RETURN

    DO J = 1, N
       JVEC(J) = 0
       IPIV(J) = J
    END DO

    ! Clear the strictly upper triangle.
    DO J = 2, N
       DO I = 1, J-1
          A(I,J) = Q_ZERO
       END DO
    END DO

    !     Factorize A as L*D*L^T using the lower triangle of A
    !
    !     K is the main loop index, increasing from 1 to N in steps of 1 or 2
    K = 1
    DO WHILE (K .LE. N)
       !     Determine rows and columns to be interchanged and whether
       !     a 1-by-1 or 2-by-2 pivot block will be used

       !     KDIAG is the index of the largest diagonal element, and
       !     DIAMAX is its absolute value
       KDIAG = IQAMAX(N-K+1, A(K,K), LDA+1) + K - 1
       DIAMAX = ABS(A(KDIAG,KDIAG))

       !     IMAX and JMAX are the row- and column-indices of the largest
       !     off-diagonal element, and OFFMAX is its absolute value
       OFFMAX = Q_ZERO
       JMAX = K
       IMAX = K+1
       DO J = K, N-1
          DO I = J+1, N
             TEMP = ABS(A(I,J))
             IF (TEMP .GT. OFFMAX) THEN
                OFFMAX = TEMP
                IMAX = I
                JMAX = J
             END IF
          END DO
       END DO

       IF (MAX(DIAMAX,OFFMAX) .EQ. Q_ZERO) THEN
          !     The rest of the matrix is zero: set INFO and return
          INFO = K
          RETURN
       END IF

       IF (DIAMAX .GE. (Q_ALPHA * OFFMAX)) THEN
          !     Use 1-by-1 pivot block
          KSTEP = 1
          KP = KDIAG
       ELSE
          !     Use 2-by-2 pivot block
          KSTEP = 2
          KP = JMAX
       END IF

       IF (KP .NE. K) THEN
          !     Interchange rows and columns K and KP in the trailing
          !     submatrix A(k:n,k:n)
          IF (KP .LT. N) CALL QSWAP(N-KP, A(KP+1,K), 1, A(KP+1,KP), 1)
          CALL QSWAP(KP-K-1, A(K+1,K), 1, A(KP,K+1), LDA)
          T = A(K,K)
          A(K,K) = A(KP,KP)
          A(KP,KP) = T
       END IF

       IF ((KSTEP .EQ. 2) .AND. (IMAX .NE. (K+1))) THEN
          !     Interchange rows and columns K+1 and IMAX in the trailing
          !     submatrix A(k:n,k:n)
          IF (IMAX .LT. N) CALL QSWAP(N-IMAX, A(IMAX+1,K+1), 1, A(IMAX+1,IMAX), 1)
          CALL QSWAP(IMAX-K-2, A(K+2,K+1), 1, A(IMAX,K+2), LDA)
          T = A(K+1,K+1)
          A(K+1,K+1) = A(IMAX,IMAX)
          A(IMAX,IMAX) = T
          T = A(IMAX,K)
          A(IMAX,K) = A(K+1,K)
          A(K+1,K) = T
       END IF

       !     Update the trailing submatrix
       IF (KSTEP .EQ. 1) THEN
          !     1-by-1 pivot block D(k): column k now holds
          !
          !     W(k+1:n,k) = L(k+1:n,k) * sqrt( abs(D(k)) ),
          !     W(k,k) = L(k,k) * sqrt( abs(D(k)) ) * J(k),
          !
          !     where L(k:n,k) is the k-th column of L, and
          !     J(k) = sign( D(k) )
          !
          !     Perform a rank-1 update of A(k+1:n,k+1:n) as
          !
          !     A := A - L(k)*J(k,k)*L(k)^T = A - W(k)*1/D(k)*W(k)^T
          R1 = Q_ONE / A(K,K)
          IF (K .LT. N) CALL QSYR(UPLO, N-K, -R1, A(K+1,K), 1, A(K+1,K+1), LDA)

          !     Compute the k-th diagonal element of the matrix J
          IF (R1 .GE. Q_ZERO) THEN
             JVEC(K) = 1
          ELSE
             JVEC(K) = -1
          END IF

          !     Store K(k) in column k
          R1 = SQRT(ABS(R1))
          !     Singer modif: IF k < n test added!
          IF (K .LT. N) CALL QSCAL(N-K, R1, A(K+1,K), 1)
          A(K,K) = JVEC(K) / R1
       ELSE
          !     2-by-2 pivot block D(k): let
          !
          !     D(k) = Q(k)^T * X(k) * Q(k)
          !
          !     be the eigendecomposition of D(k), X(k) = diag(R1,R2).
          !     Columns k and k-1 now hold
          !
          !     ( W(k+2:n,k) W(k+2:n,k+1) ) =
          !
          !     ( L(k+2:n,k) L(k+2:n,k+1) )*sqrt(abs(X(k)))*Q(k)^T,
          !
          !     W(k:k+1,k:k+1) =
          !
          !     L(k:k+1,k:k+1)*Q(k)*inv(sqrt(abs(X(k))))*J(k),
          !
          !     where L(k) and L(k+1) are the k-th and (k+1)-st columns
          !     of L, and J(k) = diag( sign(R1), sign(R2) ).
          !
          !     Perform a rank-2 update of A(k+2:n,k+2:n) as
          !
          !     A := A - ( L(k) L(k+1) )*J(k)*( L(k) L(k+1) )^T
          !     = A - ( W(k) W(k+1) )*inv(D(k))*( W(k) W(k+1) )^T
          !
          !     Convert this to two rank-1 updates by using the eigen-
          !     decomposition of D(k)
          CALL QLAEV2(A(K,K), A(K+1,K), A(K+1,K+1), R1, R2, C, S)
          R1 = Q_ONE / R1
          R2 = Q_ONE / R2
          CALL QROT(N-K-1, A(K+2,K), 1, A(K+2,K+1), 1, C, S)
          CALL QSYR(UPLO, N-K-1, -R1, A( K+2, K ), 1, A(K+2,K+2), LDA)
          CALL QSYR(UPLO, N-K-1, -R2, A( K+2, K+1 ), 1, A(K+2,K+2), LDA)

          !     Compute the k-th and (k+1)-st diagonal element of the matrix J
          IF (R1 .GE. Q_ZERO) THEN
             JVEC(K) = 1
             JVEC(K+1) = -1
          ELSE
             JVEC(K) = -1
             JVEC(K+1) = 1
          END IF

          !     Store U(k) and U(k+1) in columns k and k+1
          R1 = SQRT(ABS(R1))
          R2 = SQRT(ABS(R2))
          CALL QSCAL(N-K-1, R1, A(K+2,K), 1)
          CALL QSCAL(N-K-1, R2, A(K+2,K+1), 1)

          R1 = JVEC(K) / R1
          R2 = JVEC(K+1) / R2
          A(K,K) = C * R1
          A(K,K+1) = -S * R2
          A(K+1,K) = S * R1
          A(K+1,K+1) = C * R2
       END IF

       !     Store details of the interchanges in IPIV
       IF (KSTEP .EQ. 1) THEN
          IPIV(K) = KP
       ELSE
          IPIV(K) = -KP
          IPIV(K+1) = -IMAX
       END IF

       !     Increase K and return to the start of the main loop
       K = K + KSTEP
    END DO
  END SUBROUTINE QSYJF2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE QSYBPC(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, INFO)
    !     Modified by Singers, January 8, 2006.
    !
    !     Purpose
    !     =======
    !
    !     QSYBPC computes the modified Bunch-Parlett factorization of a N-by-N
    !     symmetric matrix A
    !
    !     A = G * J * G^T.
    !
    !     Arguments
    !     =========
    !
    !     N       (input) INTEGER
    !     Order of the input matrix A, N >= 0.
    !
    !     A       (input/output) REAL(KIND=WP) array, dimension (LDA,N)
    !     On entry, the symmetric matrix A.  The leading N-by-N lower triangular
    !     part of A contains the lower triangular part of the matrix A.
    !
    !     On exit, A contains the matrix G.
    !
    !     LDA     (input) INTEGER
    !     The leading dimension of the array A.  LDA >= max(1,N).
    !
    !     NRANK   (output) INTEGER
    !     Contains the rank of A.
    !
    !     JVEC    (output) INTEGER array, dimension(N)
    !     Contains the diagonal of the matrix J; JVEC( I ) = 1 or -1.
    !     If NRANK < N, only the first NRANK values are set.
    !
    !     IPIV    (workspace) INTEGER array, dimension(N)
    !
    !     INFO    (output) INTEGER
    !     = 0:  successful exit - the first N columns of the array A
    !     contain the matrix G (full column rank).
    !     < 0:  if INFO = -i, the i-th argument had an illegal value;
    !     > 0:  some eigenvalues are zero and INFO specifies the
    !     rank of A.
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    REAL(KIND=WP), INTENT(INOUT) :: A(LDA,N)
    INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), INFO

    INTEGER :: I, INFOD, K, KP

    ! EXTERNAL :: QSYJF2
    EXTERNAL :: QSWAP

    NRANK = 0
    NPLUS = 0
    N2PIV = 0

    !     Test the input arguments
    INFO = 0
    IF (LDA .LT. MAX(1,N)) INFO = -3
    IF (N .LT. 0) INFO = -1
    IF (INFO .NE. 0) RETURN

    !     Quick return, if possible.
    IF (N .EQ. 0) RETURN

    !     Compute the factorization A = L*J*L^H, where L is a product
    !     of permutation and lower block triangular matrices with
    !     1-by-1 and 2-by-2 diagonal blocks, and J is diagonal with
    !     diagonal elements equal to 1 or -1.
    CALL QSYJF2(N, A, LDA, JVEC, IPIV, INFOD)

    !     Set NRANK to the rank of A
    IF (INFOD .EQ. 0) THEN
       NRANK = N
    ELSE
       NRANK = INFOD - 1
    END IF

    DO I = 1, NRANK
       IF (JVEC(I) .EQ. 1) NPLUS = NPLUS + 1
       IF (IPIV(I) .LT. 0) N2PIV = N2PIV + 1
    END DO
    IF (MOD(N2PIV,2) .NE. 0) ERROR STOP 'QSYBPC: IPIV with an odd number of -1s'
    N2PIV = N2PIV / 2

    K = NRANK
    DO WHILE (K .GE. 1)
       IF (IPIV(K) .GT. 0) THEN
          !     1 x 1 diagonal block

          !     Interchange rows K and IPIV(K).
          KP = IPIV(K)
          IF (KP .NE. K) CALL QSWAP(N-K+1, A(K,K), LDA, A(KP,K), LDA)

          K = K - 1
       ELSE
          !     2 x 2 diagonal block

          !     Interchange rows K and -IPIV(K).
          KP = -IPIV(K)
          IF (KP .NE. K) CALL QSWAP((N-K)+2, A(K,K-1), LDA, A(KP,K-1), LDA)
          
          !     Interchange rows K-1 and -IPIV(K-1).
          KP = -IPIV(K-1)
          IF (KP .NE. (K-1)) CALL QSWAP((N-K)+2, A(K-1,K-1), LDA, A(KP,K-1), LDA)

          K = K - 2
       END IF
    END DO

    !     If column rank defect occured, set INFO = RANK
    IF (NRANK .LT. N) INFO = NRANK
  END SUBROUTINE QSYBPC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE QJPART(NROW, NCOLR, G, LDG, JVEC, NPLUS, IPL, INVP)
    !     Purpose
    !     =======
    !
    !     Transforms the G*J*G^T factorization into G_1*J_part*G_1^T
    !     factorization, with J_part partitioned as  J_part = ( I, -I ).
    !     Reorders the columns of G and the elements of JVEC.
    !     NPLUS is the number of elements in JVEC equal to 1.
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NROW, NCOLR, LDG, NPLUS
    REAL(KIND=WP), INTENT(INOUT) :: G(LDG,NCOLR)
    INTEGER, INTENT(INOUT) :: JVEC(NROW)
    INTEGER, INTENT(OUT) :: IPL(NROW), INVP(NROW)

    INTEGER :: I, IPLUS, IMINUS, IP, JTEMP

    EXTERNAL :: QSWAP

    DO I = NCOLR + 1, NROW
       JVEC(I) = 0
       IPL(I) = 0
       INVP(I) = 0
    END DO

    !     Early return - all JVEC( I ) have the same sign.
    IF ((NPLUS .EQ. 0) .OR. (NPLUS .EQ. NCOLR)) RETURN

    !     Set permutation IPL, where IPL( I ) holds the current place
    !     of the final I-th column.
    !     The following algorithm preserves the relative order of columns
    !     with the same sign in JVEC.
    IPLUS = 0
    IMINUS = NPLUS
    DO I = 1, NCOLR
       IF (JVEC(I) .EQ. 1) THEN
          IPLUS = IPLUS + 1
          IPL(IPLUS) = I
       ELSE
          IMINUS = IMINUS + 1
          IPL(IMINUS) = I
       END IF
    END DO

    !     Invert the permutation IPL and store it in INVP.
    DO I = 1, NCOLR
       INVP(IPL(I)) = I
    END DO

    DO I = 1, NCOLR
       !     Swap columns G( I ) and G( IPL( I ) ).
       !     Also swap the corresponding elements in JVEC.
       IF (IPL(I) .NE. I) THEN
          IP = IPL(I)

          CALL QSWAP(NROW, G(1,I), 1, G(1,IP), 1)
          JTEMP = JVEC(I)
          JVEC(I) = JVEC(IP)
          JVEC(IP) = JTEMP

          INVP(IP) = INVP(I)
          IPL(INVP(I)) = IP
       END IF
    END DO
  END SUBROUTINE QJPART

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE QSIF(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, IPL, INVP, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), IPL(N), INVP(N), INFO
    REAL(KIND=WP), INTENT(INOUT) :: A(LDA,N)

    ! EXTERNAL :: QSYBPC, QJPART

    CALL QSYBPC(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, INFO)
    IF (INFO .NE. 0) RETURN
    CALL QJPART(N, NRANK, A, LDA, JVEC, NPLUS, IPL, INVP)
  END SUBROUTINE QSIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Hermitian Indefinite Factorization

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XROTM_RR(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
    IMPLICIT NONE

    CHARACTER, INTENT(IN) :: SIDE
    INTEGER, INTENT(IN) :: INCX, INCY, N
    REAL(KIND=WP), INTENT(IN) :: C1, C2
    REAL(KIND=WP), INTENT(IN) :: S1
    REAL(KIND=WP), INTENT(IN) :: S2
    COMPLEX(KIND=WP), INTENT(INOUT) :: ZX(*), ZY(*)
    INTEGER, INTENT(OUT) :: INFO

    LOGICAL :: RIGHT
    COMPLEX(KIND=WP) :: W, Z
    INTEGER :: I, KX, KY
#include "xrotm_xx.F90"
  END SUBROUTINE XROTM_RR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XROTM_RC(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
    IMPLICIT NONE

    CHARACTER, INTENT(IN) :: SIDE
    INTEGER, INTENT(IN) :: INCX, INCY, N
    REAL(KIND=WP), INTENT(IN) :: C1, C2
    REAL(KIND=WP), INTENT(IN) :: S1
    COMPLEX(KIND=WP), INTENT(IN) :: S2
    COMPLEX(KIND=WP), INTENT(INOUT) :: ZX(*), ZY(*)
    INTEGER, INTENT(OUT) :: INFO

    LOGICAL :: RIGHT
    COMPLEX(KIND=WP) :: W, Z
    INTEGER :: I, KX, KY
#include "xrotm_xx.F90"
  END SUBROUTINE XROTM_RC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XROTM_CR(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
    IMPLICIT NONE
#include "qx_wp.fi"

    CHARACTER, INTENT(IN) :: SIDE
    INTEGER, INTENT(IN) :: INCX, INCY, N
    REAL(KIND=WP), INTENT(IN) :: C1, C2
    COMPLEX(KIND=WP), INTENT(IN) :: S1
    REAL(KIND=WP), INTENT(IN) :: S2
    COMPLEX(KIND=WP), INTENT(INOUT) :: ZX(*), ZY(*)
    INTEGER, INTENT(OUT) :: INFO

    LOGICAL :: RIGHT
    COMPLEX(KIND=WP) :: W, Z
    INTEGER :: I, KX, KY
#include "xrotm_xx.F90"
  END SUBROUTINE XROTM_CR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XROTM_CC(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
    IMPLICIT NONE

    CHARACTER, INTENT(IN) :: SIDE
    INTEGER, INTENT(IN) :: INCX, INCY, N
    REAL(KIND=WP), INTENT(IN) :: C1, C2
    COMPLEX(KIND=WP), INTENT(IN) :: S1
    COMPLEX(KIND=WP), INTENT(IN) :: S2
    COMPLEX(KIND=WP), INTENT(INOUT) :: ZX(*), ZY(*)
    INTEGER, INTENT(OUT) :: INFO

    LOGICAL :: RIGHT
    COMPLEX(KIND=WP) :: W, Z
    INTEGER :: I, KX, KY
#include "xrotm_xx.F90"
  END SUBROUTINE XROTM_CC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XROTM(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
    IMPLICIT NONE

    CHARACTER, INTENT(IN) :: SIDE
    INTEGER, INTENT(IN) :: INCX, INCY, N
    REAL(KIND=WP), INTENT(IN) :: C1, C2
    COMPLEX(KIND=WP), INTENT(IN) :: S1, S2
    COMPLEX(KIND=WP), INTENT(INOUT) :: ZX(*), ZY(*)
    INTEGER, INTENT(OUT) :: INFO

    ! EXTERNAL :: XROTM_RR, XROTM_RC, XROTM_CR, XROTM_CC

    IF (AIMAG(S1) .EQ. Q_ZERO) THEN
       ! Rx
       IF (AIMAG(S2) .EQ. Q_ZERO) THEN
          ! RR
          CALL XROTM_RR(SIDE, N, ZX, INCX, ZY, INCY, C1, REAL(S1), REAL(S2), C2, INFO)
       ELSE
          ! RC
          CALL XROTM_RC(SIDE, N, ZX, INCX, ZY, INCY, C1, REAL(S1), S2, C2, INFO)
       END IF
    ELSE
       ! Cx
       IF (AIMAG(S2) .EQ. Q_ZERO) THEN
          ! CR
          CALL XROTM_CR(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, REAL(S2), C2, INFO)
       ELSE
          ! CC
          CALL XROTM_CC(SIDE, N, ZX, INCX, ZY, INCY, C1, S1, S2, C2, INFO)
       END IF
    END IF
  END SUBROUTINE XROTM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XSWAP1(N,K, A, LDA, P,Q)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N,K, LDA, P,Q
    COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,N)

    INTEGER :: I, J
    COMPLEX(KIND=WP) :: T

    IF (N .LE. 0) RETURN

    IF (P .EQ. Q) THEN
       A(P,P) = CMPLX(REAL(A(P,P)), Q_ZERO, WP)
       RETURN
    END IF

    !     Interchange rows and columns P and Q in A
    DO I = Q+1, N
       T = A(I,Q)
       A(I,Q) = A(I,P)
       A(I,P) = T
    END DO
    DO J = 1, (Q-P)-1
       I = P+J
       T = CONJG(A(Q,I))
       A(Q,I) = CONJG(A(I,P))
       A(I,P) = T
    END DO
    DO J = K, P-1
       T = A(Q,J)
       A(Q,J) = A(P,J)
       A(P,J) = T
    END DO

    A(Q,P) = CONJG(A(Q,P))
    T = CMPLX(REAL(A(P,P)), Q_ZERO, WP)
    A(P,P) = CMPLX(REAL(A(Q,Q)), Q_ZERO, WP)
    A(Q,Q) = T
  END SUBROUTINE XSWAP1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XSWAP2(N,K, A, LDA, P,Q, R,S)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N,K, LDA, P,Q, R,S
    COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,N)

    ! EXTERNAL :: XSWAP1

    IF (N .LE. 0) RETURN

    CALL XSWAP1(N,K, A, LDA, P,Q)
    CALL XSWAP1(N,K, A, LDA, R,S)
  END SUBROUTINE XSWAP2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XHEJF2(N, A, LDA, JVEC, IPIV, INFO)
    !     Modified by Singers, January 8, 2006.
    !
    !     Proposal by Ivan Slapnicar
    !     University of Split, Croatia
    !     slap@split.fesb.hr
    !     December 15, 1993
    !
    !     Purpose
    !     =======
    !
    !     XHEJF2 computes the factorization of a Hermitian matrix A using
    !     a modification of the Bunch-Kaufman diagonal pivoting method:
    !
    !     A = L*J*L^H
    !
    !     where L is a product of permutation and lower block triangular
    !     matrices with 1-by-1 and 2-by-2 diagonal blocks, and J is diagonal,
    !     with diagonal elements equal to 1 or -1
    !     (see below for further details).
    !
    !     This is the unblocked version of the algorithm, calling Level 2 BLAS.
    !
    !     Arguments
    !     =========
    !
    !     N       (input) INTEGER
    !     The order of the matrix A.  N >= 0.
    !
    !     A       (input/output) COMPLEX(KIND=WP) array, dimension (LDA,N)
    !     On entry, the Hermitian matrix A.  The leading N-by-N lower
    !     triangular part of A contains the lower triangular part of the
    !     matrix A.  The strict upper triangular part will be destroyed!
    !
    !     On exit, the multipliers used to obtain the block triangular
    !     factor L (see below for further details).
    !
    !     LDA     (input) INTEGER
    !     The leading dimension of the array A.  LDA >= max(1,N).
    !
    !     JVEC    (output) INTEGER array, dimension(N)
    !     The diagonal of the matrix J.
    !
    !     IPIV    (output) INTEGER array, dimension (N)
    !     Details of the interchanges and the block structure of D.
    !     If IPIV(k) > 0, then rows and columns k and IPIV(k) were
    !     interchanged and in the k-th step a 1-by-1 diagonal block
    !     was used (see below for further details).
    !     If IPIV(k) < 0 and IPIV(k+1) < 0, then rows and columns k and
    !     -IPIV(k) and k+1 and -IPIV(k+1) were interchanged, and in the
    !     k-th step a 2-by-2 diagonal block was used.
    !
    !     INFO    (output) INTEGER
    !     = 0: successful exit
    !     < 0: if INFO = -k, the k-th argument had an illegal value
    !     If INFO = k, then in the k-th step the trailing submatrix
    !     A(k:n,k:n) was exactly zero, and the rank of A equals k-1.
    !
    !     Further Details
    !     ===============
    !
    !     A = L*J*L^H, where L = P(1)*L(1)* ... *P(k)*L(k)* ...,
    !     i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
    !     n in steps of 1 or 2, and J is a diagonal matrix with diagonal
    !     elements equal to 1 or -1.  P(k) is a permutation matrix as defined
    !     by IPIV(k), and L(k) is a lower block triangular matrix, such that
    !
    !             (   I    0     0   )  k-1
    !     L(k) =  (   0    d     0   )  s
    !             (   0    v     I   )  n-k-s+1
    !                k-1   s  n-k-s+1
    !
    !     Here s = 1 or 2, and ( d ) overwrites A(k:n,k:k+s-1).
    !                          ( v )
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,N)
    INTEGER, INTENT(OUT) :: JVEC(N), IPIV(N), INFO

    INTEGER :: I, J, IMAX, JMAX, K, KDIAG, KP, KSTEP
    REAL(KIND=WP) :: DIAMAX, OFFMAX, R1, R2, C, TEMP, SAR1, SAR2, UD11, UD22
    COMPLEX(KIND=WP) :: S, UD21, UD12

    ! EXTERNAL :: XSWAP1, XSWAP2, XROTM
    EXTERNAL:: XLAEV2, XQSCAL, XHER

    INFO = 0
    IF (LDA .LT. MAX(1,N)) INFO = -3
    IF (N .LT. 0) INFO = -1
    IF (INFO .NE. 0) RETURN

    DO J = 1, N
       JVEC(J) = 0
       IPIV(J) = J
    END DO

    ! Clear the strictly upper triangle.
    DO J = 2, N
       DO I = 1, J-1
          A(I,J) = CMPLX(Q_ZERO, Q_ZERO, WP)
       END DO
    END DO

    !     Factorize A as L*D*L^H using the lower triangle of A
    !
    !     K is the main loop index, increasing from 1 to N in steps of 1 or 2
    K = 1
    DO WHILE (K .LE. N)
       !     Determine rows and columns to be interchanged and whether
       !     a 1-by-1 or 2-by-2 pivot block will be used
     
       !     KDIAG is the index of the largest diagonal element, and
       !     DIAMAX is its absolute value
       KDIAG = K
       DIAMAX = ABS(REAL(A(KDIAG,KDIAG)))
       DO J = KDIAG+1, N
          TEMP = ABS(REAL(A(J,J)))
          IF (TEMP .GT. DIAMAX) THEN
             DIAMAX = TEMP
             KDIAG = J
          END IF
       END DO

       !     IMAX and JMAX are the row- and column-indices of the largest
       !     off-diagonal element, and OFFMAX is its absolute value
       OFFMAX = Q_ZERO
       JMAX = K
       IMAX = K+1
       DO J = K, N-1
          DO I = J+1, N
             TEMP = ABS(A(I,J))
             IF (TEMP .GT. OFFMAX) THEN
                OFFMAX = TEMP
                IMAX = I
                JMAX = J
             END IF
          END DO
       END DO

       IF (MAX(DIAMAX,OFFMAX) .EQ. Q_ZERO) THEN
          !     The rest of the matrix is zero: set INFO and return
          INFO = K
          RETURN
       END IF

       IF (DIAMAX .GE. (Q_ALPHA * OFFMAX)) THEN
          !     Use 1-by-1 pivot block
          KSTEP = 1
          KP = KDIAG
       ELSE
          !     Use 2-by-2 pivot block
          KSTEP = 2
          KP = JMAX
       END IF

       !     Update the trailing submatrix
       IF (KSTEP .EQ. 1) THEN
          !     Interchange rows and columns K and KP in the trailing
          !     submatrix A(k:n,k:n)
          CALL XSWAP1(N,K, A, LDA, K,KP)
 
          !     1-by-1 pivot block D(k): column k now holds
          !
          !     W(k+1:n,k) = L(k+1:n,k) * sqrt( abs(D(k)) ),
          !     W(k,k) = L(k,k) * sqrt( abs(D(k)) ) * J(k),
          !
          !     where L(k:n,k) is the k-th column of L, and
          !     J(k) = sign( D(k) )
          !
          !     Perform a rank-1 update of A(k+1:n,k+1:n) as
          !
          !     A := A - L(k)*J(k,k)*L(k)^H = A - W(k)*1/D(k)*W(k)^H
          R1 = REAL(A(K,K))
          ! R1 <> 0 here; check for NaN
          IF (R1 .NE. R1) THEN
             INFO = K
             RETURN
          END IF
          ! check for +/-Infinity
          IF (ABS(R1) .GT. HUGE(R1)) THEN
             INFO = K
             RETURN
          END IF

          R2 = Q_ONE / R1
          ! check for overflow
          IF (ABS(R2) .GT. HUGE(R2)) THEN
             INFO = K
             RETURN
          END IF
          IF (K .LT. N) CALL XHER(UPLO, N-K, -R2, A(K+1,K), 1, A(K+1,K+1), LDA)

          !     Compute the k-th diagonal element of the matrix J
          IF (R1 .GT. Q_ZERO) THEN
             JVEC(K) = 1
             !     Store L(k) in column k
             A(K,K) = CMPLX(SQRT(R1), Q_ZERO, WP)
             R2 = SQRT(R2)
          ELSE IF (R1 .LT. Q_ZERO) THEN
             JVEC(K) = -1
             !     Store L(k) in column k
             A(K,K) = CMPLX(SQRT(-R1), Q_ZERO, WP)
             R2 = -SQRT(-R2)
          ELSE ! should never happen
             ERROR STOP 'XHEJF2: R1 = 0'
          END IF

          IF (K .LT. N) CALL XQSCAL(N-K, R2, A(K+1,K), 1)
       ELSE
          !     Interchange rows and columns K+1 and IMAX in the trailing
          !     submatrix A(k:n,k:n)
          CALL XSWAP2(N,K, A, LDA, K,KP, K+1,IMAX)

          !     2-by-2 pivot block D(k): let
          !
          !     D(k) = Q(k)**H * X(k) * Q(k)
          !
          !     be the eigendecomposition of D(k), X(k) = diag(R1,R2).
          !     Columns k and k-1 now hold
          !
          !     ( W(k+2:n,k) W(k+2:n,k+1) ) =
          !
          !     ( L(k+2:n,k) L(k+2:n,k+1) )*sqrt(abs(X(k)))*Q(k)^H,
          !
          !     W(k:k+1,k:k+1) =
          !
          !     L(k:k+1,k:k+1)*Q(k)*inv(sqrt(abs(X(k))))*J(k),
          !
          !     where L(k) and L(k+1) are the k-th and (k+1)-st columns
          !     of L, and J(k) = diag( sign(R1), sign(R2) ).
          !
          !     Perform a rank-2 update of A(k+2:n,k+2:n) as
          !
          !     A := A - ( L(k) L(k+1) )*J(k)*( L(k) L(k+1) )^H
          !     = A - ( W(k) W(k+1) )*inv(D(k))*( W(k) W(k+1) )^H
          !
          !     Convert this to two rank-1 updates by using the eigen-
          !     decomposition of D(k)
          TEMP = REAL(A(K,K))
          ! check for NaN
          IF (TEMP .NE. TEMP) THEN
             INFO = K
             RETURN
          END IF
          TEMP = REAL(A(K+1,K+1))
          ! check for NaN
          IF (TEMP .NE. TEMP) THEN
             INFO = K
             RETURN
          END IF
          TEMP = REAL(A(K+1,K))
          ! check for NaN
          IF (TEMP .NE. TEMP) THEN
             INFO = K
             RETURN
          END IF
          ! check for +/-Infinity
          IF (ABS(TEMP) .GT. HUGE(TEMP)) THEN
             INFO = K
             RETURN
          END IF
          TEMP = AIMAG(A(K+1,K))
          ! check for NaN
          IF (TEMP .NE. TEMP) THEN
             INFO = K
             RETURN
          END IF
          ! check for +/-Infinity
          IF (ABS(TEMP) .GT. HUGE(TEMP)) THEN
             INFO = K
             RETURN
          END IF

          CALL XLAEV2(A(K,K), CONJG(A(K+1,K)), A(K+1,K+1), R1, R2, C, S)
          ! -~S=-(Re(S),-Im(S))=(-Re(S),Im(S))=~(-Re(S),-Im(S))=~-S
          !     Compute the k-th and (k+1)-st diagonal element of the matrix J
          IF (R1 .GT. Q_ZERO) THEN
             JVEC(K) = 1
          ELSE IF (R1 .LT. Q_ZERO) THEN
             JVEC(K) = -1
          ELSE ! ERROR
             INFO = K
             RETURN
          END IF
          IF (R2 .GT. Q_ZERO) THEN
             JVEC(K+1) = 1
          ELSE IF (R2 .LT. Q_ZERO) THEN
             JVEC(K+1) = -1
          ELSE ! ERROR
             INFO = K
             RETURN
          END IF
          IF (JVEC(K+1) .NE. -JVEC(K)) ERROR STOP 'XHEJF2: JVEC(K+1) <> -JVEC(K)'

          SAR1 = SQRT(ABS(R1))
          SAR2 = SQRT(ABS(R2))
          !     Store L(k) and L(k+1) in columns k and k+1
          A(K,K) = CMPLX(C * SAR1, Q_ZERO, WP)
          A(K,K+1) = -CONJG(S) * SAR2
          A(K+1,K) = S * SAR1
          A(K+1,K+1) = CMPLX(C * SAR2, Q_ZERO, WP)
          IF (K .LT. (N-1)) THEN
             SAR1 = JVEC(K) / SAR1
             SAR2 = JVEC(K+1) / SAR2
             UD11 = C * SAR1
             UD21 = S * SAR1
             UD12 = -CONJG(S) * SAR2
             UD22 = C * SAR2
             CALL XROTM(SIDE, (N-K)-1, A(K+2,K), 1, A(K+2,K+1), 1, UD11, UD21, UD12, UD22, I)
             IF (I .LT. 0) ERROR STOP 'XHEJF2: XROTM'
             TEMP = -REAL(JVEC(K), WP)
             CALL XHER(UPLO, (N-K)-1, TEMP, A(K+2,K), 1, A(K+2,K+2), LDA)
             TEMP = -REAL(JVEC(K+1), WP)
             CALL XHER(UPLO, (N-K)-1, TEMP, A(K+2,K+1), 1, A(K+2,K+2), LDA)
          END IF
       END IF

       !     Store details of the interchanges in IPIV
       IF (KSTEP .EQ. 1) THEN
          IPIV(K) = KP
       ELSE
          IPIV(K) = -KP
          IPIV(K+1) = -IMAX
       END IF

       !     Increase K and return to the start of the main loop
       K = K + KSTEP
    END DO
  END SUBROUTINE XHEJF2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XHEBPC(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, INFO)
    !     Modified by Singers, January 8, 2006.
    !
    !     Purpose
    !     =======
    !
    !     XHEBPC computes the modified Bunch-Parlett factorization of a N-by-N
    !     Hermitian matrix A
    !
    !     A = G * J * G^H.
    !
    !     Arguments
    !     =========
    !
    !     N       (input) INTEGER
    !     Order of the input matrix A, N >= 0.
    !
    !     A       (input/output) COMPLEX(KIND=WP) array, dimension (LDA,N)
    !     On entry, the Hermitian matrix A.  The leading N-by-N lower triangular
    !     part of A contains the lower triangular part of the matrix A.
    !
    !     On exit, A contains the matrix G.
    !
    !     LDA     (input) INTEGER
    !     The leading dimension of the array A.  LDA >= max(1,N).
    !
    !     NRANK   (output) INTEGER
    !     Contains the rank of A.
    !
    !     JVEC    (output) INTEGER array, dimension(N)
    !     Contains the diagonal of the matrix J; JVEC( I ) = 1 or -1.
    !     If NRANK < N, only the first NRANK values are set.
    !
    !     IPIV    (workspace) INTEGER array, dimension(N)
    !
    !     INFO    (output) INTEGER
    !     = 0:  successful exit - the first N columns of the array A
    !     contain the matrix G (full column rank).
    !     < 0:  if INFO = -i, the i-th argument had an illegal value;
    !     > 0:  some eigenvalues are zero and INFO specifies the
    !     rank of A.
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,N)
    INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), INFO

    INTEGER :: I, INFOD, K, KP

    ! EXTERNAL :: XHEJF2
    EXTERNAL :: XSWAP

    NRANK = 0
    NPLUS = 0
    N2PIV = 0

    !     Test the input arguments
    INFO = 0
    IF (LDA .LT. MAX(1,N)) INFO = -3
    IF (N .LT. 0) INFO = -1
    IF (INFO .NE. 0) RETURN

    !     Quick return, if possible.
    IF (N .EQ. 0) RETURN

    !     Compute the factorization A = L*J*L^H, where L is a product
    !     of permutation and lower block triangular matrices with
    !     1-by-1 and 2-by-2 diagonal blocks, and J is diagonal with
    !     diagonal elements equal to 1 or -1.
    CALL XHEJF2(N, A, LDA, JVEC, IPIV, INFOD)

    !     Set NRANK to the rank of A
    IF (INFOD .EQ. 0) THEN
       NRANK = N
    ELSE
       NRANK = INFOD - 1
    END IF

    DO I = 1, NRANK
       IF (JVEC(I) .EQ. 1) NPLUS = NPLUS + 1
       IF (IPIV(I) .LT. 0) N2PIV = N2PIV + 1
    END DO
    IF (MOD(N2PIV,2) .NE. 0) ERROR STOP 'XHEBPC: IPIV with an odd number of -1s'
    N2PIV = N2PIV / 2

    K = NRANK
    DO WHILE (K .GE. 1)
       IF (IPIV(K) .GT. 0) THEN
          !     1 x 1 diagonal block

          !     Interchange rows K and IPIV(K).
          KP = IPIV(K)
          IF (KP .NE. K) CALL XSWAP(N-K+1, A(K,K), LDA, A(KP,K), LDA)

          K = K - 1
       ELSE
          !     2 x 2 diagonal block

          !     Interchange rows K and -IPIV(K).
          KP = -IPIV(K)
          IF (KP .NE. K) CALL XSWAP((N-K)+2, A(K,K-1), LDA, A(KP,K-1), LDA)

          !     Interchange rows K-1 and -IPIV(K-1).
          KP = -IPIV(K-1)
          IF (KP .NE. (K-1)) CALL XSWAP((N-K)+2, A(K-1,K-1), LDA, A(KP,K-1), LDA)

          K = K - 2
       END IF
    END DO

    !     If column rank defect occured, set INFO = RANK
    IF (NRANK .LT. N) INFO = NRANK
  END SUBROUTINE XHEBPC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XJPART(NROW, NCOLR, G, LDG, JVEC, NPLUS, IPL, INVP)
    !     Purpose
    !     =======
    !
    !     Transforms the G*J*G^H factorization into G_1*J_part*G_1^H
    !     factorization, with J_part partitioned as  J_part = ( I, -I ).
    !     Reorders the columns of G and the elements of JVEC.
    !     NPLUS is the number of elements in JVEC equal to 1.
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NROW, NCOLR, LDG, NPLUS
    COMPLEX(KIND=WP), INTENT(INOUT) :: G(LDG,NCOLR)
    INTEGER, INTENT(INOUT) :: JVEC(NROW)
    INTEGER, INTENT(OUT) :: IPL(NROW), INVP(NROW)

    INTEGER :: I, IPLUS, IMINUS, IP, JTEMP

    EXTERNAL :: XSWAP

    DO I = NCOLR + 1, NROW
       JVEC(I) = 0
       IPL(I) = 0
       INVP(I) = 0
    END DO

    !     Early return - all JVEC( I ) have the same sign.
    IF ((NPLUS .EQ. 0) .OR. (NPLUS .EQ. NCOLR)) RETURN

    !     Set permutation IPL, where IPL( I ) holds the current place
    !     of the final I-th column.
    !     The following algorithm preserves the relative order of columns
    !     with the same sign in JVEC.
    IPLUS = 0
    IMINUS = NPLUS
    DO I = 1, NCOLR
       IF (JVEC(I) .EQ. 1) THEN
          IPLUS = IPLUS + 1
          IPL(IPLUS) = I
       ELSE
          IMINUS = IMINUS + 1
          IPL(IMINUS) = I
       END IF
    END DO

    !     Invert the permutation IPL and store it in INVP.
    DO I = 1, NCOLR
       INVP(IPL(I)) = I
    END DO

    DO I = 1, NCOLR
       !     Swap columns G( I ) and G( IPL( I ) ).
       !     Also swap the corresponding elements in JVEC.
       IF (IPL(I) .NE. I) THEN
          IP = IPL(I)

          CALL XSWAP(NROW, G(1,I), 1, G(1,IP), 1)
          JTEMP = JVEC(I)
          JVEC(I) = JVEC(IP)
          JVEC(IP) = JTEMP

          INVP(IP) = INVP(I)
          IPL(INVP(I)) = IP
       END IF
    END DO
  END SUBROUTINE XJPART

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE XHIF(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, IPL, INVP, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, LDA
    INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), IPL(N), INVP(N), INFO
    COMPLEX(KIND=WP), INTENT(INOUT) :: A(LDA,N)

    ! EXTERNAL :: XHEBPC, XJPART

    CALL XHEBPC(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, INFO)
    IF (INFO .NE. 0) RETURN
    CALL XJPART(N, NRANK, A, LDA, JVEC, NPLUS, IPL, INVP)
  END SUBROUTINE XHIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE INDEFF

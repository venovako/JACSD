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
  !     A       (input/output) REAL(WP) array, dimension (LDA,N)
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
#include "qx_wp.fi"

  CHARACTER, PARAMETER :: UPLO = 'L'
  REAL(WP), PARAMETER :: Q_ZERO = 0.0_WP, Q_ONE = 1.0_WP, Q_ALPHA = (Q_ONE + SQRT(17.0_WP)) / 8.0_WP

  INTEGER, INTENT(IN) :: N, LDA
  REAL(WP), INTENT(INOUT) :: A(LDA,N)
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
  !     A       (input/output) REAL(WP) array, dimension (LDA,N)
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
#include "qx_wp.fi"

  INTEGER, INTENT(IN) :: N, LDA
  REAL(WP), INTENT(INOUT) :: A(LDA,N)
  INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), INFO

  INTEGER :: I, INFOD, K, KP

  EXTERNAL :: QSYJF2
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
  IF (MOD(N2PIV,2) .NE. 0) STOP 'QSYBPC: IPIV with an odd number of -1s'
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
#include "qx_wp.fi"

  INTEGER, INTENT(IN) :: NROW, NCOLR, LDG, NPLUS
  REAL(WP), INTENT(INOUT) :: G(LDG,NCOLR)
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
#include "qx_wp.fi"

  INTEGER, INTENT(IN) :: N, LDA
  INTEGER, INTENT(OUT) :: NRANK, NPLUS, N2PIV, IPIV(N), JVEC(N), IPL(N), INVP(N), INFO
  REAL(WP), INTENT(INOUT) :: A(LDA,N)

  EXTERNAL :: QSYBPC, QJPART

  CALL QSYBPC(N, A, LDA, NRANK, NPLUS, N2PIV, IPIV, JVEC, INFO)
  IF (INFO .NE. 0) RETURN
  CALL QJPART(N, NRANK, A, LDA, JVEC, NPLUS, IPL, INVP)
END SUBROUTINE QSIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

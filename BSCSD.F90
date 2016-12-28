!TODO: decide what arrays to put into FASTMEM for KNL.
MODULE BSCSD

  USE CSD
  IMPLICIT NONE

CONTAINS

  SUBROUTINE SBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO)

    IMPLICIT NONE

    CHARACTER, PARAMETER :: JOBU1 = 'Y', JOBU2 = 'Y', JOBV1T = 'Y', JOBV2T = 'Y', TRANS = 'N', SIGNS = 'N'

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT, LWORK
    INTEGER, INTENT(OUT) :: IWORK(*), INFO

    REAL, INTENT(IN) :: X(LDX,*)
    REAL, INTENT(OUT) :: THETA(*), U(LDU,*), VT(LDVT,*), WORK(*)

    EXTERNAL :: SORCSD

    CALL SORCSD(JOBU1, JOBU2, JOBV1T, JOBV2T, TRANS, SIGNS,&
         M, P, Q, X(1,1), LDX, X(1,Q+1), LDX, X(P+1,1), LDX, X(P+1,Q+1), LDX,&
         THETA, U(1,1), LDU, U(P+1,P+1), LDU, VT(1,1), LDVT, VT(Q+1,Q+1), LDVT, WORK, LWORK, IWORK, INFO)

  END SUBROUTINE SBSCSD

  SUBROUTINE SCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT
    INTEGER, INTENT(OUT) :: INFO

    REAL, INTENT(IN) :: X(LDX,*)
    REAL, INTENT(OUT) :: THETA(*), C(*), S(*), U(LDU,*), VT(LDVT,*)

    REAL, ALLOCATABLE :: WORK(:)
    INTEGER, ALLOCATABLE :: IWORK(:)

    INTEGER :: R, LWORK, IWRK(1), I
    REAL :: WRK(1)
    EQUIVALENCE (IWRK(1),I)

    LWORK = -1
    CALL SBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WRK, LWORK, IWRK, INFO)
    IF (INFO .NE. 0) RETURN
    LWORK = MAX(1,CEILING(WRK(1)))

    ALLOCATE(WORK(LWORK))
    ALLOCATE(IWORK(M))
    CALL SBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO)
    DEALLOCATE(IWORK)
    IF (INFO .EQ. 0) THEN
       R = MIN(P, M-P, Q, M-Q)
       DO I = 1, R
          CALL SSINCOS(THETA(I), S(I), C(I))
!           C(I) = COS(THETA(I))
!           S(I) = SIN(THETA(I))
       END DO
    ELSE IF (INFO .GT. 0) THEN
       C(1) = REAL(INFO)
       R = MIN(P, M-P, Q, M-Q)
       DO I = 2, R
          C(I) = WORK(I)
       END DO
    END IF
    DEALLOCATE(WORK)

  END SUBROUTINE SCLCSD

  SUBROUTINE STXCSD(M, P, Q, NT, FN, RN, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M, P, Q, NT
    CHARACTER(LEN=*), INTENT(IN) :: FN, RN
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: LDX, LDU, LDVT, R, I, J, ONT, ISTATS(4), WALLTM(3)

    REAL, ALLOCATABLE :: X(:,:), U(:,:), VT(:,:), THETA(:), C(:), S(:)

#ifndef USE_ESSL
    EXTERNAL :: SLASET
#endif

    IF (M .LT. 0) THEN
       INFO = -1
    ELSE IF (P .LT. 0) THEN
       INFO = -2
    ELSE IF (P .GT. M) THEN
       INFO = -2
    ELSE IF (Q .LT. 0) THEN
       INFO = -3
    ELSE IF (Q .GT. M) THEN
       INFO = -3
    ELSE IF (NT .LT. 0) THEN
       INFO = -4
    ELSE IF (LEN_TRIM(FN) .LE. 0) THEN
       INFO = -5
    ELSE
       INFO = 0
    END IF

    IF (INFO .NE. 0) RETURN

    J = GET_IOUNIT('N')
    CALL BIN_OPEN(J, FN, 'OLD', 'READ', INFO)
    IF (INFO .NE. 0) RETURN

    LDX = MAX(1,M)
    LDU = MAX(1,M)
    LDVT = MAX(1,M)
    R = MAX(1,MIN(P, M-P, Q, M-Q))

    ALLOCATE(X(LDX,M))

    CALL BIN_READ_2S(J, LDX, X, M, M, INFO)
    IF (INFO .NE. 0) RETURN

    CALL BIN_CLOSE(J, INFO)
    IF (INFO .NE. 0) RETURN

    ALLOCATE(U(LDU,M))
    ALLOCATE(VT(LDVT,M))

    ALLOCATE(THETA(R))
    ALLOCATE(C(R))
    ALLOCATE(S(R))

    IF (NT .GT. 0) THEN
       IF (BLAS_PREPARE() .LT. 0) THEN
          INFO = -4
          RETURN
       ELSE
          ONT = BLAS_SET_NUM_THREADS(NT)
       END IF
    END IF

    CALL TIMER_START(WALLTM)
    CALL SCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)
    CALL TIMER_STOP(WALLTM)

    J = GET_IOUNIT('O')
    WRITE (UNIT=J,FMT='(I5,A,I3,A,I5,A,I5,A,I5,A)',ADVANCE='NO') INFO, ',S,', NT, ',', M, ',', P, ',', Q, ','
    CALL TIMER_PRINT(WALLTM)

    IF (INFO .EQ. 0) THEN
       CALL SLASET('A', M-P, P, S_ZERO, S_ZERO, U(P+1,1),  LDU)
       CALL SLASET('A', P, M-P, S_ZERO, S_ZERO, U(1,P+1),  LDU)
       CALL SLASET('A', M-Q, Q, S_ZERO, S_ZERO, VT(Q+1,1), LDVT)
       CALL SLASET('A', Q, M-Q, S_ZERO, S_ZERO, VT(1,Q+1), LDVT)
       CALL SLASET('A', LDX, M, S_ZERO, S_ZERO, X,         LDX)
       IF (Q .EQ. R) THEN
          ! I = 0
          DO J = 1, R
             X(J,J) = C(J)
          END DO
          DO J = 1, R
             X(M-Q+J,J) = S(J)
          END DO
          DO J = 1, R
             X(J,M-P+J) = -S(J)
          END DO
          DO J = 1, P-Q
             X(R+J,M-P+R+J) = S_MONE
          END DO
          DO J = 1, M-P-Q
             X(P+J,Q+J) = S_ONE
          END DO
          DO J = 1, R
             X(M-Q+J,M-P+J) = C(J)
          END DO
       ELSE ! Q > R
          ! I = Q - R
          INFO = Q - R
          RETURN
       END IF
    END IF

    IF (NT .GT. 0) ONT = BLAS_SET_NUM_THREADS(ONT)
    J = BLAS_FINISH()
    IF (J .LT. 0) STOP 'BLAS_FINISH'

    IF ((INFO .GE. 0) .AND. (LEN_TRIM(RN) .GT. 0)) THEN
       ISTATS(1) = M
       ISTATS(2) = P
       ISTATS(3) = Q
       ISTATS(4) = INFO

       J = GET_IOUNIT('N')
       CALL BIN_OPEN(J, RN, 'REPLACE', 'WRITE', I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1I(J, ISTATS, 4, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1S(J, THETA, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1S(J, C, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       IF (INFO .EQ. 0) THEN
          CALL BIN_WRITE_1S(J, S, R, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2S(J, LDX, X, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2S(J, LDU, U, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2S(J, LDVT, VT, M, M, INFO)
          IF (INFO .NE. 0) RETURN
       END IF

       CALL BIN_CLOSE(J, INFO)
       IF (INFO .NE. 0) RETURN
    END IF

    DEALLOCATE(S)
    DEALLOCATE(C)
    DEALLOCATE(THETA)
    DEALLOCATE(VT)
    DEALLOCATE(U)
    DEALLOCATE(X)

  END SUBROUTINE STXCSD

  SUBROUTINE DBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO)

    IMPLICIT NONE

    CHARACTER, PARAMETER :: JOBU1 = 'Y', JOBU2 = 'Y', JOBV1T = 'Y', JOBV2T = 'Y', TRANS = 'N', SIGNS = 'N'

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT, LWORK
    INTEGER, INTENT(OUT) :: IWORK(*), INFO

    DOUBLE PRECISION, INTENT(IN) :: X(LDX,*)
    DOUBLE PRECISION, INTENT(OUT) :: THETA(*), U(LDU,*), VT(LDVT,*), WORK(*)

    EXTERNAL :: DORCSD

    CALL DORCSD(JOBU1, JOBU2, JOBV1T, JOBV2T, TRANS, SIGNS,&
         M, P, Q, X(1,1), LDX, X(1,Q+1), LDX, X(P+1,1), LDX, X(P+1,Q+1), LDX,&
         THETA, U(1,1), LDU, U(P+1,P+1), LDU, VT(1,1), LDVT, VT(Q+1,Q+1), LDVT, WORK, LWORK, IWORK, INFO)

  END SUBROUTINE DBSCSD

  SUBROUTINE DCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT
    INTEGER, INTENT(OUT) :: INFO

    DOUBLE PRECISION, INTENT(IN) :: X(LDX,*)
    DOUBLE PRECISION, INTENT(OUT) :: THETA(*), C(*), S(*), U(LDU,*), VT(LDVT,*)

    DOUBLE PRECISION, ALLOCATABLE :: WORK(:)
    INTEGER, ALLOCATABLE :: IWORK(:)

    INTEGER :: R, LWORK, IWRK(1), I
    DOUBLE PRECISION :: WRK(1)
    EQUIVALENCE (IWRK(1),I)

    LWORK = -1
    CALL DBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WRK, LWORK, IWRK, INFO)
    IF (INFO .NE. 0) RETURN
    LWORK = MAX(1,CEILING(WRK(1)))

    ALLOCATE(WORK(LWORK))
    ALLOCATE(IWORK(M))
    CALL DBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO)
    DEALLOCATE(IWORK)
    IF (INFO .EQ. 0) THEN
       R = MIN(P, M-P, Q, M-Q)
       DO I = 1, R
          CALL DSINCOS(THETA(I), S(I), C(I))
!           C(I) = COS(THETA(I))
!           S(I) = SIN(THETA(I))
       END DO
    ELSE IF (INFO .GT. 0) THEN
       C(1) = DBLE(INFO)
       R = MIN(P, M-P, Q, M-Q)
       DO I = 2, R
          C(I) = WORK(I)
       END DO
    END IF
    DEALLOCATE(WORK)

  END SUBROUTINE DCLCSD

  SUBROUTINE DTXCSD(M, P, Q, NT, FN, RN, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M, P, Q, NT
    CHARACTER(LEN=*), INTENT(IN) :: FN, RN
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: LDX, LDU, LDVT, R, I, J, ONT, ISTATS(4), WALLTM(3)

    DOUBLE PRECISION, ALLOCATABLE :: X(:,:), U(:,:), VT(:,:), THETA(:), C(:), S(:)

#ifndef USE_ESSL
    EXTERNAL :: DLASET
#endif

    IF (M .LT. 0) THEN
       INFO = -1
    ELSE IF (P .LT. 0) THEN
       INFO = -2
    ELSE IF (P .GT. M) THEN
       INFO = -2
    ELSE IF (Q .LT. 0) THEN
       INFO = -3
    ELSE IF (Q .GT. M) THEN
       INFO = -3
    ELSE IF (NT .LT. 0) THEN
       INFO = -4
    ELSE IF (LEN_TRIM(FN) .LE. 0) THEN
       INFO = -5
    ELSE
       INFO = 0
    END IF

    IF (INFO .NE. 0) RETURN

    J = GET_IOUNIT('N')
    CALL BIN_OPEN(J, FN, 'OLD', 'READ', INFO)
    IF (INFO .NE. 0) RETURN

    LDX = MAX(1,M)
    LDU = MAX(1,M)
    LDVT = MAX(1,M)
    R = MAX(1,MIN(P, M-P, Q, M-Q))

    ALLOCATE(X(LDX,M))

    CALL BIN_READ_2D(J, LDX, X, M, M, INFO)
    IF (INFO .NE. 0) RETURN

    CALL BIN_CLOSE(J, INFO)
    IF (INFO .NE. 0) RETURN

    ALLOCATE(U(LDU,M))
    ALLOCATE(VT(LDVT,M))

    ALLOCATE(THETA(R))
    ALLOCATE(C(R))
    ALLOCATE(S(R))

    IF (NT .GT. 0) THEN
       IF (BLAS_PREPARE() .LT. 0) THEN
          INFO = -4
          RETURN
       ELSE
          ONT = BLAS_SET_NUM_THREADS(NT)
       END IF
    END IF

    CALL TIMER_START(WALLTM)
    CALL DCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)
    CALL TIMER_STOP(WALLTM)

    J = GET_IOUNIT('O')
    WRITE (UNIT=J,FMT='(I5,A,I3,A,I5,A,I5,A,I5,A)',ADVANCE='NO') INFO, ',D,', NT, ',', M, ',', P, ',', Q, ','
    CALL TIMER_PRINT(WALLTM)

    IF (INFO .EQ. 0) THEN
       CALL DLASET('A', M-P, P, D_ZERO, D_ZERO, U(P+1,1),  LDU)
       CALL DLASET('A', P, M-P, D_ZERO, D_ZERO, U(1,P+1),  LDU)
       CALL DLASET('A', M-Q, Q, D_ZERO, D_ZERO, VT(Q+1,1), LDVT)
       CALL DLASET('A', Q, M-Q, D_ZERO, D_ZERO, VT(1,Q+1), LDVT)
       CALL DLASET('A', LDX, M, D_ZERO, D_ZERO, X,         LDX)
       IF (Q .EQ. R) THEN
          ! I = 0
          DO J = 1, R
             X(J,J) = C(J)
          END DO
          DO J = 1, R
             X(M-Q+J,J) = S(J)
          END DO
          DO J = 1, R
             X(J,M-P+J) = -S(J)
          END DO
          DO J = 1, P-Q
             X(R+J,M-P+R+J) = D_MONE
          END DO
          DO J = 1, M-P-Q
             X(P+J,Q+J) = D_ONE
          END DO
          DO J = 1, R
             X(M-Q+J,M-P+J) = C(J)
          END DO
       ELSE ! Q > R
          ! I = Q - R
          INFO = Q - R
          RETURN
       END IF
    END IF

    IF (NT .GT. 0) ONT = BLAS_SET_NUM_THREADS(ONT)
    J = BLAS_FINISH()
    IF (J .LT. 0) STOP 'BLAS_FINISH'

    IF ((INFO .GE. 0) .AND. (LEN_TRIM(RN) .GT. 0)) THEN
       ISTATS(1) = M
       ISTATS(2) = P
       ISTATS(3) = Q
       ISTATS(4) = INFO

       J = GET_IOUNIT('N')
       CALL BIN_OPEN(J, RN, 'REPLACE', 'WRITE', I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1I(J, ISTATS, 4, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1D(J, THETA, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1D(J, C, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       IF (INFO .EQ. 0) THEN
          CALL BIN_WRITE_1D(J, S, R, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2D(J, LDX, X, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2D(J, LDU, U, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2D(J, LDVT, VT, M, M, INFO)
          IF (INFO .NE. 0) RETURN
       END IF

       CALL BIN_CLOSE(J, INFO)
       IF (INFO .NE. 0) RETURN
    END IF

    DEALLOCATE(S)
    DEALLOCATE(C)
    DEALLOCATE(THETA)
    DEALLOCATE(VT)
    DEALLOCATE(U)
    DEALLOCATE(X)

  END SUBROUTINE DTXCSD

  SUBROUTINE CBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, RWORK, LRWORK, IWORK, INFO)

    IMPLICIT NONE

    CHARACTER, PARAMETER :: JOBU1 = 'Y', JOBU2 = 'Y', JOBV1T = 'Y', JOBV2T = 'Y', TRANS = 'N', SIGNS = 'N'

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT, LWORK,LRWORK
    INTEGER, INTENT(OUT) :: IWORK(*), INFO

    COMPLEX, INTENT(IN) :: X(LDX,*)
    COMPLEX, INTENT(OUT) :: THETA(*), U(LDU,*), VT(LDVT,*), WORK(*)
    REAL, INTENT(OUT) :: RWORK(*)

    EXTERNAL :: CUNCSD

    CALL CUNCSD(JOBU1, JOBU2, JOBV1T, JOBV2T, TRANS, SIGNS,&
         M, P, Q, X(1,1), LDX, X(1,Q+1), LDX, X(P+1,1), LDX, X(P+1,Q+1), LDX,&
         THETA, U(1,1), LDU, U(P+1,P+1), LDU, VT(1,1), LDVT, VT(Q+1,Q+1), LDVT,&
         WORK, LWORK, RWORK, LRWORK, IWORK, INFO)

  END SUBROUTINE CBSCSD

  SUBROUTINE CCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT
    INTEGER, INTENT(OUT) :: INFO

    COMPLEX, INTENT(IN) :: X(LDX,*)
    COMPLEX, INTENT(OUT) :: THETA(*), C(*), S(*), U(LDU,*), VT(LDVT,*)

    COMPLEX, ALLOCATABLE :: WORK(:)
    REAL, ALLOCATABLE :: RWORK(:)
    INTEGER, ALLOCATABLE :: IWORK(:)

    INTEGER :: R, LWORK, LRWORK, IWRK(1), I
    COMPLEX :: WRK(1)
    REAL :: RWRK(2)
    EQUIVALENCE (IWRK(1),I)

    LWORK = -1
    LRWORK = -1
    CALL CBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WRK, LWORK, RWRK, LRWORK, IWRK, INFO)
    IF (INFO .NE. 0) RETURN
    LWORK = MAX(1,CEILING(REAL(WRK(1))))
    LRWORK = MAX(1,CEILING(RWRK(1)))

    ALLOCATE(WORK(LWORK))
    ALLOCATE(RWORK(LRWORK))
    ALLOCATE(IWORK(M))
    CALL CBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, RWORK, LRWORK, IWORK, INFO)
    DEALLOCATE(IWORK)
    IF (INFO .EQ. 0) THEN
       R = MIN(P, M-P, Q, M-Q)
       DO I = 1, R
          IF (AIMAG(THETA(I)) .EQ. S_ZERO) THEN
             CALL SSINCOS(REAL(THETA(I)), RWRK(1), RWRK(2))
             C(I) = RWRK(2)
             S(I) = RWRK(1)
          ELSE
             C(I) = COS(THETA(I))
             S(I) = SIN(THETA(I))
          END IF
       END DO
    ELSE IF (INFO .GT. 0) THEN
       C(1) = REAL(INFO)
       S(1) = REAL(INFO)
       R = MIN(P, M-P, Q, M-Q)
       DO I = 2, R
          C(I) = WORK(I)
          S(I) = RWORK(I)
       END DO
    END IF
    DEALLOCATE(RWORK)
    DEALLOCATE(WORK)

  END SUBROUTINE CCLCSD

  SUBROUTINE CTXCSD(M, P, Q, NT, FN, RN, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M, P, Q, NT
    CHARACTER(LEN=*), INTENT(IN) :: FN, RN
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: LDX, LDU, LDVT, R, I, J, ONT, ISTATS(4), WALLTM(3)

    COMPLEX, ALLOCATABLE :: X(:,:), U(:,:), VT(:,:), THETA(:), C(:), S(:)

#ifndef USE_ESSL
    EXTERNAL :: CLASET
#endif

    IF (M .LT. 0) THEN
       INFO = -1
    ELSE IF (P .LT. 0) THEN
       INFO = -2
    ELSE IF (P .GT. M) THEN
       INFO = -2
    ELSE IF (Q .LT. 0) THEN
       INFO = -3
    ELSE IF (Q .GT. M) THEN
       INFO = -3
    ELSE IF (NT .LT. 0) THEN
       INFO = -4
    ELSE IF (LEN_TRIM(FN) .LE. 0) THEN
       INFO = -5
    ELSE
       INFO = 0
    END IF

    IF (INFO .NE. 0) RETURN

    J = GET_IOUNIT('N')
    CALL BIN_OPEN(J, FN, 'OLD', 'READ', INFO)
    IF (INFO .NE. 0) RETURN

    LDX = MAX(1,M)
    LDU = MAX(1,M)
    LDVT = MAX(1,M)
    R = MAX(1,MIN(P, M-P, Q, M-Q))

    ALLOCATE(X(LDX,M))

    CALL BIN_READ_2C(J, LDX, X, M, M, INFO)
    IF (INFO .NE. 0) RETURN

    CALL BIN_CLOSE(J, INFO)
    IF (INFO .NE. 0) RETURN

    ALLOCATE(U(LDU,M))
    ALLOCATE(VT(LDVT,M))

    ALLOCATE(THETA(R))
    ALLOCATE(C(R))
    ALLOCATE(S(R))

    IF (NT .GT. 0) THEN
       IF (BLAS_PREPARE() .LT. 0) THEN
          INFO = -4
          RETURN
       ELSE
          ONT = BLAS_SET_NUM_THREADS(NT)
       END IF
    END IF

    CALL TIMER_START(WALLTM)
    CALL CCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)
    CALL TIMER_STOP(WALLTM)

    J = GET_IOUNIT('O')
    WRITE (UNIT=J,FMT='(I5,A,I3,A,I5,A,I5,A,I5,A)',ADVANCE='NO') INFO, ',C,', NT, ',', M, ',', P, ',', Q, ','
    CALL TIMER_PRINT(WALLTM)

    IF (INFO .EQ. 0) THEN
       CALL CLASET('A', M-P, P, C_ZERO, C_ZERO, U(P+1,1),  LDU)
       CALL CLASET('A', P, M-P, C_ZERO, C_ZERO, U(1,P+1),  LDU)
       CALL CLASET('A', M-Q, Q, C_ZERO, C_ZERO, VT(Q+1,1), LDVT)
       CALL CLASET('A', Q, M-Q, C_ZERO, C_ZERO, VT(1,Q+1), LDVT)
       CALL CLASET('A', LDX, M, C_ZERO, C_ZERO, X,         LDX)
       IF (Q .EQ. R) THEN
          ! I = 0
          DO J = 1, R
             X(J,J) = C(J)
          END DO
          DO J = 1, R
             X(M-Q+J,J) = S(J)
          END DO
          DO J = 1, R
             X(J,M-P+J) = -S(J)
          END DO
          DO J = 1, P-Q
             X(R+J,M-P+R+J) = C_MONE
          END DO
          DO J = 1, M-P-Q
             X(P+J,Q+J) = C_ONE
          END DO
          DO J = 1, R
             X(M-Q+J,M-P+J) = C(J)
          END DO
       ELSE ! Q > R
          ! I = Q - R
          INFO = Q - R
          RETURN
       END IF
    END IF

    IF (NT .GT. 0) ONT = BLAS_SET_NUM_THREADS(ONT)
    J = BLAS_FINISH()
    IF (J .LT. 0) STOP 'BLAS_FINISH'

    IF ((INFO .GE. 0) .AND. (LEN_TRIM(RN) .GT. 0)) THEN
       ISTATS(1) = M
       ISTATS(2) = P
       ISTATS(3) = Q
       ISTATS(4) = INFO

       J = GET_IOUNIT('N')
       CALL BIN_OPEN(J, RN, 'REPLACE', 'WRITE', I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1I(J, ISTATS, 4, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1C(J, THETA, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1C(J, C, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       IF (INFO .EQ. 0) THEN
          CALL BIN_WRITE_1C(J, S, R, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2C(J, LDX, X, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2C(J, LDU, U, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2C(J, LDVT, VT, M, M, INFO)
          IF (INFO .NE. 0) RETURN
       END IF

       CALL BIN_CLOSE(J, INFO)
       IF (INFO .NE. 0) RETURN
    END IF

    DEALLOCATE(S)
    DEALLOCATE(C)
    DEALLOCATE(THETA)
    DEALLOCATE(VT)
    DEALLOCATE(U)
    DEALLOCATE(X)

  END SUBROUTINE CTXCSD

  SUBROUTINE ZBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, RWORK, LRWORK, IWORK, INFO)

    IMPLICIT NONE

    CHARACTER, PARAMETER :: JOBU1 = 'Y', JOBU2 = 'Y', JOBV1T = 'Y', JOBV2T = 'Y', TRANS = 'N', SIGNS = 'N'

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT, LWORK,LRWORK
    INTEGER, INTENT(OUT) :: IWORK(*), INFO

    DOUBLE COMPLEX, INTENT(IN) :: X(LDX,*)
    DOUBLE COMPLEX, INTENT(OUT) :: THETA(*), U(LDU,*), VT(LDVT,*), WORK(*)
    DOUBLE PRECISION, INTENT(OUT) :: RWORK(*)

    EXTERNAL :: ZUNCSD

    CALL ZUNCSD(JOBU1, JOBU2, JOBV1T, JOBV2T, TRANS, SIGNS,&
         M, P, Q, X(1,1), LDX, X(1,Q+1), LDX, X(P+1,1), LDX, X(P+1,Q+1), LDX,&
         THETA, U(1,1), LDU, U(P+1,P+1), LDU, VT(1,1), LDVT, VT(Q+1,Q+1), LDVT,&
         WORK, LWORK, RWORK, LRWORK, IWORK, INFO)

  END SUBROUTINE ZBSCSD

  SUBROUTINE ZCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M,P,Q, LDX,LDU,LDVT
    INTEGER, INTENT(OUT) :: INFO

    DOUBLE COMPLEX, INTENT(IN) :: X(LDX,*)
    DOUBLE COMPLEX, INTENT(OUT) :: THETA(*), C(*), S(*), U(LDU,*), VT(LDVT,*)

    DOUBLE COMPLEX, ALLOCATABLE :: WORK(:)
    DOUBLE PRECISION, ALLOCATABLE :: RWORK(:)
    INTEGER, ALLOCATABLE :: IWORK(:)

    INTEGER :: R, LWORK, LRWORK, IWRK(1), I
    DOUBLE COMPLEX :: WRK(1)
    DOUBLE PRECISION :: RWRK(2)
    EQUIVALENCE (IWRK(1),I)

    LWORK = -1
    LRWORK = -1
    CALL ZBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WRK, LWORK, RWRK, LRWORK, IWRK, INFO)
    IF (INFO .NE. 0) RETURN
    LWORK = MAX(1,CEILING(DBLE(WRK(1))))
    LRWORK = MAX(1,CEILING(RWRK(1)))

    ALLOCATE(WORK(LWORK))
    ALLOCATE(RWORK(LRWORK))
    ALLOCATE(IWORK(M))
    CALL ZBSCSD(M, P, Q, X, LDX, THETA, U, LDU, VT, LDVT, WORK, LWORK, RWORK, LRWORK, IWORK, INFO)
    DEALLOCATE(IWORK)
    IF (INFO .EQ. 0) THEN
       R = MIN(P, M-P, Q, M-Q)
       DO I = 1, R
          IF (AIMAG(THETA(I)) .EQ. D_ZERO) THEN
             CALL DSINCOS(DBLE(THETA(I)), RWRK(1), RWRK(2))
             C(I) = RWRK(2)
             S(I) = RWRK(1)
          ELSE
             C(I) = COS(THETA(I))
             S(I) = SIN(THETA(I))
          END IF
       END DO
    ELSE IF (INFO .GT. 0) THEN
       C(1) = DBLE(INFO)
       S(1) = DBLE(INFO)
       R = MIN(P, M-P, Q, M-Q)
       DO I = 2, R
          C(I) = WORK(I)
          S(I) = RWORK(I)
       END DO
    END IF
    DEALLOCATE(RWORK)
    DEALLOCATE(WORK)

  END SUBROUTINE ZCLCSD

  SUBROUTINE ZTXCSD(M, P, Q, NT, FN, RN, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M, P, Q, NT
    CHARACTER(LEN=*), INTENT(IN) :: FN, RN
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: LDX, LDU, LDVT, R, I, J, ONT, ISTATS(4), WALLTM(3)

    DOUBLE COMPLEX, ALLOCATABLE :: X(:,:), U(:,:), VT(:,:), THETA(:), C(:), S(:)

#ifndef USE_ESSL
    EXTERNAL :: ZLASET
#endif

    IF (M .LT. 0) THEN
       INFO = -1
    ELSE IF (P .LT. 0) THEN
       INFO = -2
    ELSE IF (P .GT. M) THEN
       INFO = -2
    ELSE IF (Q .LT. 0) THEN
       INFO = -3
    ELSE IF (Q .GT. M) THEN
       INFO = -3
    ELSE IF (NT .LT. 0) THEN
       INFO = -4
    ELSE IF (LEN_TRIM(FN) .LE. 0) THEN
       INFO = -5
    ELSE
       INFO = 0
    END IF

    IF (INFO .NE. 0) RETURN

    J = GET_IOUNIT('N')
    CALL BIN_OPEN(J, FN, 'OLD', 'READ', INFO)
    IF (INFO .NE. 0) RETURN

    LDX = MAX(1,M)
    LDU = MAX(1,M)
    LDVT = MAX(1,M)
    R = MAX(1,MIN(P, M-P, Q, M-Q))

    ALLOCATE(X(LDX,M))

    CALL BIN_READ_2Z(J, LDX, X, M, M, INFO)
    IF (INFO .NE. 0) RETURN

    CALL BIN_CLOSE(J, INFO)
    IF (INFO .NE. 0) RETURN

    ALLOCATE(U(LDU,M))
    ALLOCATE(VT(LDVT,M))

    ALLOCATE(THETA(R))
    ALLOCATE(C(R))
    ALLOCATE(S(R))

    IF (NT .GT. 0) THEN
       IF (BLAS_PREPARE() .LT. 0) THEN
          INFO = -4
          RETURN
       ELSE
          ONT = BLAS_SET_NUM_THREADS(NT)
       END IF
    END IF

    CALL TIMER_START(WALLTM)
    CALL ZCLCSD(M, P, Q, X, LDX, THETA, C, S, U, LDU, VT, LDVT, INFO)
    CALL TIMER_STOP(WALLTM)

    J = GET_IOUNIT('O')
    WRITE (UNIT=J,FMT='(I5,A,I3,A,I5,A,I5,A,I5,A)',ADVANCE='NO') INFO, ',Z,', NT, ',', M, ',', P, ',', Q, ','
    CALL TIMER_PRINT(WALLTM)

    IF (INFO .EQ. 0) THEN
       CALL ZLASET('A', M-P, P, Z_ZERO, Z_ZERO, U(P+1,1),  LDU)
       CALL ZLASET('A', P, M-P, Z_ZERO, Z_ZERO, U(1,P+1),  LDU)
       CALL ZLASET('A', M-Q, Q, Z_ZERO, Z_ZERO, VT(Q+1,1), LDVT)
       CALL ZLASET('A', Q, M-Q, Z_ZERO, Z_ZERO, VT(1,Q+1), LDVT)
       CALL ZLASET('A', LDX, M, Z_ZERO, Z_ZERO, X,         LDX)
       IF (Q .EQ. R) THEN
          ! I = 0
          DO J = 1, R
             X(J,J) = C(J)
          END DO
          DO J = 1, R
             X(M-Q+J,J) = S(J)
          END DO
          DO J = 1, R
             X(J,M-P+J) = -S(J)
          END DO
          DO J = 1, P-Q
             X(R+J,M-P+R+J) = Z_MONE
          END DO
          DO J = 1, M-P-Q
             X(P+J,Q+J) = Z_ONE
          END DO
          DO J = 1, R
             X(M-Q+J,M-P+J) = C(J)
          END DO
       ELSE ! Q > R
          ! I = Q - R
          INFO = Q - R
          RETURN
       END IF
    END IF

    IF (NT .GT. 0) ONT = BLAS_SET_NUM_THREADS(ONT)
    J = BLAS_FINISH()
    IF (J .LT. 0) STOP 'BLAS_FINISH'

    IF ((INFO .GE. 0) .AND. (LEN_TRIM(RN) .GT. 0)) THEN
       ISTATS(1) = M
       ISTATS(2) = P
       ISTATS(3) = Q
       ISTATS(4) = INFO

       J = GET_IOUNIT('N')
       CALL BIN_OPEN(J, RN, 'REPLACE', 'WRITE', I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1I(J, ISTATS, 4, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1Z(J, THETA, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       CALL BIN_WRITE_1Z(J, C, R, I)
       IF (I .NE. 0) THEN
          INFO = I
          RETURN
       END IF

       IF (INFO .EQ. 0) THEN
          CALL BIN_WRITE_1Z(J, S, R, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2Z(J, LDX, X, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2Z(J, LDU, U, M, M, INFO)
          IF (INFO .NE. 0) RETURN

          CALL BIN_WRITE_2Z(J, LDVT, VT, M, M, INFO)
          IF (INFO .NE. 0) RETURN
       END IF

       CALL BIN_CLOSE(J, INFO)
       IF (INFO .NE. 0) RETURN
    END IF

    DEALLOCATE(S)
    DEALLOCATE(C)
    DEALLOCATE(THETA)
    DEALLOCATE(VT)
    DEALLOCATE(U)
    DEALLOCATE(X)

  END SUBROUTINE ZTXCSD

END MODULE BSCSD

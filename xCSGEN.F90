PROGRAM xCSGEN

  USE CSD
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'

  INTEGER, PARAMETER :: MYSEED(4) = (/ 4, 3, 2, 1 /)
  INTEGER, PARAMETER :: MYDIST = 1 ! uniform(0,1)
  REAL(WP), PARAMETER :: QPI_2 = SCALE(ATAN2(1.0_WP, 1.0_WP), 1)

  INTEGER :: M, P, Q, R
  CHARACTER(LEN=256) :: F

  INTEGER :: ISEED(8), IDIST
  EQUIVALENCE (ISEED(5),IDIST), (ISEED(6),M), (ISEED(7),P), (ISEED(8),Q)

  INTEGER :: LDU, LDVT, LDX
  INTEGER :: IOU, INFO
  INTEGER :: I, J, K
  LOGICAL :: LSWP
  REAL(WP) :: QW

  REAL(WP), ALLOCATABLE :: QT(:), QC(:), QS(:), U(:,:), X(:,:), VT(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: T(:), DX(:,:)

  INTEGER, INTRINSIC :: MAX, MIN
  REAL(WP), INTRINSIC :: COS, SIN
  DOUBLE PRECISION, INTRINSIC :: DBLE
  REAL(WP), EXTERNAL :: QLARND
  EXTERNAL :: QGEMM, QLAROR, QLASET

  CALL READCL(M, P, F, INFO)
  IF (INFO .NE. 0) STOP 'xCSGEN.exe M P F'
  IF (M .LE. 0) STOP 'M <= 0'
  IF (P .LE. 0) STOP 'P <= 0'
  IF (P .GE. M) STOP 'P >= M'
  IF ((2 * P) .GT. M) STOP '2P > M'
  Q = P
  R = MAX(1,MIN(P, M-P, Q, M-Q))

  IOU = GET_IOUNIT('N')
  IF (IOU .LT. 0) STOP 'IOU < 0'

  DO I = 1, 4
     ISEED(I) = MYSEED(I)
  END DO
  IDIST = MYDIST

  CALL BIN_OPEN(IOU, (TRIM(F)//'.tcs'), 'REPLACE', 'WRITE', INFO)
  IF (INFO .NE. 0) STOP 'BIN_OPEN(tcs)'

  CALL BIN_WRITE_1I(IOU, ISEED, 8, INFO)
  IF (INFO .NE. 0) STOP 'BIN_WRITE_1I'

  WRITE (*,'(A)') 'Generating (theta,cos,sin)... '

  K = 3 * MAX(P, M-P)
  ALLOCATE(QT(K))

  ALLOCATE(QC(P))
  ALLOCATE(QS(P))

  DO I = 1, P
     QT(I) = QLARND(IDIST, ISEED) * QPI_2
     QC(I) = COS(QT(I))
     QS(I) = SIN(QT(I))
  END DO

  K = P
  LSWP = .TRUE.
  DO WHILE (LSWP)
     K = K - 1
     LSWP = .FALSE.

     DO I = 1, K
        J = I + 1
        IF (QC(I) .LT. QC(J)) THEN
           QW = QT(J)
           QT(J) = QT(I)
           QT(I) = QW

           QW = QC(J)
           QC(J) = QC(I)
           QC(I) = QW

           QW = QS(J)
           QS(J) = QS(I)
           QS(I) = QW

           LSWP = .TRUE.
        END IF
     END DO
  END DO

  DO I = 1, P-1
     IF (QC(I) .LT. QC(I+1)) STOP 'ZLO'
  END DO
  ALLOCATE(T(P))

  DO I = 1, P
     T(I) = DBLE(QT(I))
  END DO

  CALL BIN_WRITE_1D(IOU, T, P, INFO)
  IF (INFO .NE. 0) STOP 'BIN_WRITE_1D(T)'

  DO I = 1, P
     T(I) = DBLE(QC(I))
  END DO
  CALL BIN_WRITE_1D(IOU, T, P, INFO)
  IF (INFO .NE. 0) STOP 'BIN_WRITE_1D(C)'

  DO I = 1, P
     T(I) = DBLE(QS(I))
  END DO
  CALL BIN_WRITE_1D(IOU, T, P, INFO)
  IF (INFO .NE. 0) STOP 'BIN_WRITE_1D(S)'

  DEALLOCATE(T)
  CALL BIN_CLOSE(IOU, INFO)
  IF (INFO .NE. 0) STOP 'BIN_CLOSE(tcs)'

  WRITE (*,'(A)') 'Generating CS...'

  LDX = M
  ALLOCATE(X(LDX,M))

  CALL QLASET('A', LDX, M, 0.0_WP, 0.0_WP, X, LDX)
  DO J = 1, R
     X(J,J) = QC(J)
  END DO
  DO J = 1, R
     X(M-Q+J,J) = QS(J)
  END DO
  DO J = 1, R
     X(J,M-P+J) = -QS(J)
  END DO
  DO J = 1, P-Q
     X(R+J,M-P+R+J) = -1.0_WP
  END DO
  DO J = 1, M-P-Q
     X(P+J,Q+J) = 1.0_WP
  END DO
  DO J = 1, R
     X(M-Q+J,M-P+J) = QC(J)
  END DO

  DEALLOCATE(QS)
  DEALLOCATE(QC)

  WRITE (*,'(A)') 'Generating U...'

  LDU = M
  ALLOCATE(U(LDU,M))

  CALL QLAROR('L', 'I',   P,   P, U(  1,  1), LDU, ISEED, QT, INFO)
  IF (INFO .NE. 0) STOP 'QLAROR(U1)'
  CALL QLAROR('L', 'I', M-P, M-P, U(P+1,P+1), LDU, ISEED, QT, INFO)
  IF (INFO .NE. 0) STOP 'QLAROR(U2)'
  CALL QLASET('A', M-P, P, 0.0_WP, 0.0_WP, U(P+1,1),  LDU)
  CALL QLASET('A', P, M-P, 0.0_WP, 0.0_WP, U(1,P+1),  LDU)

  WRITE (*,'(A)') 'Multiplying U*CS...'

  LDVT = M
  ALLOCATE(VT(LDVT,M))

  ! U*X -> VT
  CALL QGEMM('N', 'N', M, M, M, 1.0_WP, U, LDU, X, LDX, 0.0_WP, VT, LDVT)

  WRITE (*,'(A)') 'Generating VT...'

  CALL QLAROR('R', 'I',   Q,   Q, U(  1,  1), LDU, ISEED, QT, INFO)
  IF (INFO .NE. 0) STOP 'QLAROR(V1T)'
  CALL QLAROR('R', 'I', M-Q, M-Q, U(Q+1,Q+1), LDU, ISEED, QT, INFO)
  IF (INFO .NE. 0) STOP 'QLAROR(V2T)'

  DEALLOCATE(QT)

  CALL QLASET('A', M-Q, Q, 0.0_WP, 0.0_WP, U(Q+1,1), LDU)
  CALL QLASET('A', Q, M-Q, 0.0_WP, 0.0_WP, U(1,Q+1), LDU)

  WRITE (*,'(A)') 'Multiplying X=(U*CS)*VT...'

  CALL QGEMM('N', 'N', M, M, M, 1.0_WP, VT, LDVT, U, LDU, 0.0_WP, X, LDX)

  DEALLOCATE(VT)
  DEALLOCATE(U)

  WRITE (*,'(A)') 'Storing DBLE(X)...'

  ALLOCATE(DX(LDX,M))
  DO J = 1, M
     DO I = 1, M
        DX(I,J) = DBLE(X(I,J))
     END DO
  END DO

  DEALLOCATE(X)

  CALL BIN_OPEN(IOU, TRIM(F), 'REPLACE', 'WRITE', INFO)
  IF (INFO .NE. 0) STOP 'BIN_OPEN'

  CALL BIN_WRITE_2D(IOU, LDX, DX, M, M, INFO)
  IF (INFO .NE. 0) STOP 'BIN_WRITE_2D(DX)'

  DEALLOCATE(DX)

  CALL BIN_CLOSE(IOU, INFO)
  IF (INFO .NE. 0) STOP 'BIN_CLOSE'

CONTAINS

  SUBROUTINE READCL(M, P, F, INFO)

    IMPLICIT NONE

    INTEGER, INTENT(OUT) :: M, P, INFO
    CHARACTER(LEN=*), INTENT(OUT) :: F

#ifndef USE_IBM
    INTEGER, INTRINSIC :: COMMAND_ARGUMENT_COUNT
#endif
    INTRINSIC :: GET_COMMAND_ARGUMENT

    INFO = COMMAND_ARGUMENT_COUNT()
    IF (INFO .EQ. 3) THEN
       CALL GET_COMMAND_ARGUMENT(1, F)
       READ (F,*) M

       CALL GET_COMMAND_ARGUMENT(2, F)
       READ (F,*) P

       CALL GET_COMMAND_ARGUMENT(3, F)
    END IF
    INFO = INFO - 3

  END SUBROUTINE READCL

END PROGRAM xCSGEN

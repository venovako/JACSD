MODULE LAMGEN
  USE SEED
  IMPLICIT NONE

  DOUBLE PRECISION, PARAMETER, PRIVATE :: ZERO = 0.0D0, ONE = 1.0D0

CONTAINS

  SUBROUTINE DGENLAM(N, K, ISEED, IDIST, EPS, SCAL, LAM, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, K, IDIST
    INTEGER, INTENT(INOUT) :: ISEED(4)
    DOUBLE PRECISION, INTENT(IN) :: EPS, SCAL
    DOUBLE PRECISION, INTENT(OUT) :: LAM(N)
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: I

    DOUBLE PRECISION, EXTERNAL :: DLARND

    CALL SEEDOK(ISEED, I)

    IF (N .LT. 0) THEN
       INFO = -1
    ELSE IF (K .LT. 0) THEN
       INFO = -2
    ELSE IF (K .GT. (N - MOD(N, 2))) THEN
       INFO = -2
    ELSE IF (MOD(K, 2) .NE. 0) THEN
       INFO = -2
    ELSE IF (I .NE. 0) THEN
       INFO = -3
    ELSE IF ((IDIST .LT. 1) .OR. (IDIST .GT. 3)) THEN
       INFO = -4
    ELSE IF (EPS .LT. ZERO) THEN
       INFO = -5
    ELSE IF (SCAL .EQ. ZERO) THEN
       INFO = -6
    ELSE ! all OK
       INFO = 0
    END IF
    IF (INFO .NE. 0) RETURN

    I = 1
    DO WHILE (I .LT. K)
       LAM(I) = DLARND(IDIST, ISEED)
       IF (ABS(LAM(I)) .GT. EPS) THEN
          LAM(I+1) = -LAM(I)
          I = I + 2
       END IF
    END DO

    IF (SCAL .NE. ONE) THEN
       DO I = 1, K
          LAM(I) = LAM(I) * SCAL
       END DO
    END IF

    DO I = K+1, N
       LAM(I) = ZERO
    END DO
  END SUBROUTINE DGENLAM

  SUBROUTINE DTXTLAM(FN, N, K, LAM, INFO)
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: FN
    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(OUT) :: LAM(N)
    INTEGER, INTENT(OUT) :: K, INFO

    INTEGER :: I, U
    LOGICAL :: FEX

    K = 0
    INQUIRE(FILE=TRIM(FN), EXIST=FEX)

    IF (.NOT. FEX) THEN
       INFO = -1
    ELSE IF (N .LT. 0) THEN
       INFO = -2
    ELSE
       INFO = 0
    END IF
    IF (INFO .NE. 0) RETURN

    U = -1
    OPEN(NEWUNIT=U, FILE=TRIM(FN), ACTION='READ', STATUS='OLD')

    I = 1
    DO WHILE (I .LE. N)
       READ (U,*) LAM(I)
       IF (ABS(LAM(I)) .GT. ZERO) THEN
          IF (I .LT. N) THEN
             LAM(I+1) = -LAM(I)
             I = I + 2
             K = K + 2
          ELSE ! error
             INFO = I
             GOTO 1
          END IF
       END IF
    END DO

1   CLOSE(U)
  END SUBROUTINE DTXTLAM

  SUBROUTINE ZGENLAM(N, K, ISEED, IDIST, EPS, SCAL, LAM, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N, K, IDIST
    INTEGER, INTENT(INOUT) :: ISEED(4)
    DOUBLE PRECISION, INTENT(IN) :: EPS, SCAL
    DOUBLE PRECISION, INTENT(OUT) :: LAM(N)
    INTEGER, INTENT(OUT) :: INFO

    INTEGER :: I

    DOUBLE PRECISION, EXTERNAL :: DLARND

    CALL SEEDOK(ISEED, I)

    IF (N .LT. 0) THEN
       INFO = -1
    ELSE IF (K .LT. 0) THEN
       INFO = -2
    ELSE IF (K .GT. N) THEN
       INFO = -2
    ELSE IF (I .NE. 0) THEN
       INFO = -3
    ELSE IF ((IDIST .LT. 1) .OR. (IDIST .GT. 3)) THEN
       INFO = -4
    ELSE IF (EPS .LT. ZERO) THEN
       INFO = -5
    ELSE IF (SCAL .EQ. ZERO) THEN
       INFO = -6
    ELSE ! all OK
       INFO = 0
    END IF
    IF (INFO .NE. 0) RETURN

    I = 1
    DO WHILE (I .LE. K)
       LAM(I) = DLARND(IDIST, ISEED)
       IF (ABS(LAM(I)) .GT. EPS) I = I + 1
    END DO

    IF (SCAL .NE. ONE) THEN
       DO I = 1, K
          LAM(I) = LAM(I) * SCAL
       END DO
    END IF

    DO I = K+1, N
       LAM(I) = ZERO
    END DO
  END SUBROUTINE ZGENLAM

  SUBROUTINE ZTXTLAM(FN, N, K, LAM, INFO)
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: FN
    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(OUT) :: LAM(N)
    INTEGER, INTENT(OUT) :: K, INFO

    INTEGER :: I, U
    LOGICAL :: FEX

    K = 0
    INQUIRE(FILE=TRIM(FN), EXIST=FEX)

    IF (.NOT. FEX) THEN
       INFO = -1
    ELSE IF (N .LT. 0) THEN
       INFO = -2
    ELSE
       INFO = 0
    END IF
    IF (INFO .NE. 0) RETURN

    U = -1
    OPEN(NEWUNIT=U, FILE=TRIM(FN), ACTION='READ', STATUS='OLD')

    DO I = 1, N
       READ (U,*) LAM(I)
       IF (ABS(LAM(I)) .GT. ZERO) K = K + 1
    END DO

    CLOSE(U)
  END SUBROUTINE ZTXTLAM

END MODULE LAMGEN

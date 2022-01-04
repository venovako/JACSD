! N>=2 singular values s \in [2**A, 2**B]
! lg(s) uniformly spread in [A,B]
! condition = 2**(MAX(A,B)-MIN(A,B))
! see `man shuf` for a random permutation
PROGRAM GENTXT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  IMPLICIT NONE

  INTEGER, PARAMETER :: WP = QX_WP
  DOUBLE PRECISION, PARAMETER :: ONE = 1.0D0

  INTEGER :: N, A, B, INFO, I
  REAL(KIND=WP) :: QA, QB, QC, QD, QS

  CALL READCL(N, A, B, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I2)',ADVANCE='NO') INFO
     ERROR STOP ' invalid command line'
  END IF

  QA = A
  QB = B
  QC = QA
  QD = (QB - QA) / (N - 1)

  WRITE (*,1) SCALE(ONE, A)
  DO I = 1, N-2
     QC = QC + QD
     QS = 2**QC
     WRITE (*,1) DBLE(QS)
  END DO
  WRITE (*,1) SCALE(ONE, B)

1 FORMAT(ES24.17E3)

CONTAINS

  SUBROUTINE READCL(N, A, B, INFO)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: N, A, B, INFO

    CHARACTER(LEN=11) :: CAS

    IF (COMMAND_ARGUMENT_COUNT() .NE. 3) THEN
       WRITE (ERROR_UNIT,*) 'gentxt.exe N A B'
       INFO = 1
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(1, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -1
       RETURN
    END IF
    READ (CAS,*) N
    IF (N .LE. 1) THEN
       INFO = -1
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(2, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -2
       RETURN
    END IF
    READ (CAS,*) A

    CALL GET_COMMAND_ARGUMENT(3, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -3
       RETURN
    END IF
    READ (CAS,*) B

    INFO = 0
  END SUBROUTINE READCL

END PROGRAM GENTXT

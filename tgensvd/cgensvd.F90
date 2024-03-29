PROGRAM CGENSVD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
  USE BIO
  USE SEED
  USE SVGEN
  USE DATGEN
  IMPLICIT NONE

  INTEGER, PARAMETER :: WP = QX_WP
  INTEGER, PARAMETER :: FNL = 253
  REAL, PARAMETER :: ZERO = 0.0

  ! command-line parameters
  INTEGER :: ISIGMA, SEEDI, M, N, INFO
  REAL :: EPS, SCAL ! optional
  CHARACTER(LEN=FNL) :: FSIGMA, FIL

  INTEGER :: ISEED(4), U

  REAL, ALLOCATABLE :: SS(:)
  COMPLEX(KIND=WP), ALLOCATABLE :: XWORK(:)

  COMPLEX, ALLOCATABLE :: SG(:,:)
  COMPLEX(KIND=WP), ALLOCATABLE :: XG(:,:)

  CALL READCL(ISIGMA, FSIGMA, SEEDI, M, N, FIL, EPS, SCAL, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I2)',ADVANCE='NO') INFO
     ERROR STOP ' READCL'
  END IF

  CALL SEEDIX(SEEDI, ISEED, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I2)',ADVANCE='NO') INFO
     ERROR STOP ' SEEDIX'
  END IF

  ALLOCATE(SS(N))
  IF (ISIGMA .NE. 0) THEN
     CALL SGENSV(N, ISEED, ISIGMA, EPS, SCAL, SS, INFO)
  ELSE ! read the file
     CALL STXTSV(FSIGMA, N, SS, INFO)
  END IF
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' SIGMA'
  END IF

  ALLOCATE(SG(M,N))
  ALLOCATE(XG(M,N))
  ALLOCATE(XWORK(3*M))

  CALL CGENDAT(M, N, ISEED, SS, XG, SG, XWORK, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I2)',ADVANCE='NO') INFO
     ERROR STOP ' CGENDAT'
  END IF

  DEALLOCATE(XWORK)
  DEALLOCATE(XG)

  U = -1
  CALL BIO_OPEN(U, TRIM(FIL)//'.G', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_OPEN(G)'
  END IF
  CALL BIO_WRITE_C2(U, M, N, SG, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_WRITE_C2(G)'
  END IF
  CALL BIO_CLOSE(U, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_CLOSE(G)'
  END IF
  DEALLOCATE(SG)

  U = -1
  CALL BIO_OPEN(U, TRIM(FIL)//'.S', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_OPEN(S)'
  END IF
  CALL BIO_WRITE_S1(U, N, SS, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_WRITE_S1(S)'
  END IF
  CALL BIO_CLOSE(U, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') INFO
     ERROR STOP ' BIO_CLOSE(S)'
  END IF
  DEALLOCATE(SS)

CONTAINS

  SUBROUTINE READCL(ISIGMA, FSIGMA, SEEDI, M, N, FIL, EPS, SCAL, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(OUT) :: ISIGMA, SEEDI, M, N, INFO
    REAL, INTENT(OUT) :: EPS, SCAL
    CHARACTER(LEN=*), INTENT(OUT) :: FSIGMA, FIL

    INTEGER, PARAMETER :: NRQP = 5
    INTEGER :: NXTA
    CHARACTER(LEN=FNL) :: CAS

    ISIGMA = 0
    SEEDI = 0
    M = 0
    N = 0
    INFO = 0

    EPS = ZERO
    SCAL = ZERO

    FSIGMA = ''
    FIL = ''

    NXTA = NRQP
    CAS = ''

    IF (COMMAND_ARGUMENT_COUNT() .LT. NRQP) THEN
       WRITE (OUTPUT_UNIT,*) 'cgensvd.exe SIGMA SEEDIX M N FILE [ EPS SCALE ]'
       WRITE (OUTPUT_UNIT,*) '>> COMMAND LINE (INPUT) ARGUMENTS <<'
       WRITE (OUTPUT_UNIT,*) 'SIGMA : \Sigma; 1=uniform(0,1), 3=abs(normal(0,1)), or FNAME'
       WRITE (OUTPUT_UNIT,*) ' FNAME: max 253 chars, >= N lines [each line = a positive value]'
       WRITE (OUTPUT_UNIT,*) 'SEEDIX: index of hard-coded pRNG seed (see seedix.F90); 1 or 2'
       WRITE (OUTPUT_UNIT,*) 'M     : number of rows of the output matrix: >= N'
       WRITE (OUTPUT_UNIT,*) 'N     : number of columns order of the output matrix: > 0'
       WRITE (OUTPUT_UNIT,*) 'FILE  : output file name prefix: max 253 chars'
       WRITE (OUTPUT_UNIT,*) ' EPS  : \sigma''_i survives iff |\sigma''_i| > EPS'
       WRITE (OUTPUT_UNIT,*) ' SCALE: final \sigma_i = \sigma''_i * SCALE'
       WRITE (OUTPUT_UNIT,*) '<< OUTPUT DATASETS >>'
       WRITE (OUTPUT_UNIT,*) 'FILE.S: real(N); \Sigma'
       WRITE (OUTPUT_UNIT,*) 'FILE.G: complex(M,N); G = U \Sigma V^H'
       INFO = 1
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(1, FSIGMA, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -1
       RETURN
    END IF
    IF (TRIM(FSIGMA) .EQ. '1') THEN
       ISIGMA = 1
       FSIGMA = ''
    ELSE IF (TRIM(FSIGMA) .EQ. '3') THEN
       ISIGMA = 3
       FSIGMA = ''
    END IF

    CALL GET_COMMAND_ARGUMENT(2, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -2
       RETURN
    END IF
    READ (CAS,*) SEEDI
    IF (SEEDI .LE. 0) THEN
       INFO = -2
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(3, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -3
       RETURN
    END IF
    READ (CAS,*) M
    IF (M .LE. 0) THEN
       INFO = -3
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(4, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -4
       RETURN
    END IF
    READ (CAS,*) N
    IF ((N .LE. 0) .OR. (N .GT. M)) THEN
       INFO = -4
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(5, FIL, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -5
       RETURN
    END IF
    IF (LEN_TRIM(FIL) .LE. 0) THEN
       INFO = -5
       RETURN
    END IF

    IF (ISIGMA .NE. 0) THEN
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) EPS
       IF (EPS .LT. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) SCAL
       IF (SCAL .LE. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
    END IF
  END SUBROUTINE READCL
END PROGRAM CGENSVD

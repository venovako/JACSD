PROGRAM DGENHSVD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ERROR_UNIT
  IMPLICIT NONE
#include "qx_wp.fi"

  INTEGER, PARAMETER :: FNL = 256
  INTEGER, PARAMETER :: IOMSGLEN = 66
  DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0

  ! command-line parameters
  INTEGER :: SEED, N, IDIST, INFO
  DOUBLE PRECISION :: EPS, SCAL
  CHARACTER(LEN=FNL) :: LAMBDA, FIL

  INTEGER :: ISEED(4), P

  INTEGER, ALLOCATABLE :: J(:), IWORK(:)
  DOUBLE PRECISION, ALLOCATABLE :: DLAMBDA(:)
  REAL(WP), ALLOCATABLE :: QLAMBDA(:), QWORK(:)

  DOUBLE PRECISION, ALLOCATABLE :: DA(:,:)
  REAL(WP), ALLOCATABLE :: QA(:,:)

  EXTERNAL :: SEEDIX, DGENLAM, DTXTLAM, DGENDAT

  CALL READCL(LAMBDA, SEED, N, FIL, IDIST, EPS, SCAL, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'READCL'
  END IF

  CALL SEEDIX(SEED, ISEED, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'SEEDIX'
  END IF

  ALLOCATE(DLAMBDA(N))
  IF (IDIST .NE. 0) THEN
     CALL DGENLAM(N, ISEED, IDIST, EPS, SCAL, DLAMBDA, P, INFO)
  ELSE
     CALL DTXTLAM(LAMBDA, N, DLAMBDA, P, INFO)
  END IF
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'LAMBDA'
  END IF

  ALLOCATE(DA(N,N))
  ALLOCATE(J(N))

  ALLOCATE(QA(N,N))
  ALLOCATE(QLAMBDA(N))
  DO P = 1, N
     QLAMBDA(P) = REAL(DLAMBDA(P),WP)
  END DO
  P = 3 * N
  ALLOCATE(IWORK(P))
  P = 2 * N
  ALLOCATE(QWORK(P))

  CALL DGENDAT(N, ISEED, QLAMBDA, QA, DA, J, IWORK, QWORK, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'DGENDAT'
  END IF

  DEALLOCATE(QWORK)
  DEALLOCATE(IWORK)
  DEALLOCATE(QLAMBDA)
  DEALLOCATE(QA)

  CALL BIO_OPEN(P, TRIM(FIL)//'.J', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(J)'
  END IF
  CALL BIO_WRITE_I1(P, N, J, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_I1(J)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(J)'
  END IF
  DEALLOCATE(J)

  CALL BIO_OPEN(P, TRIM(FIL)//'.Y', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(Y)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DA, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(Y)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(Y)'
  END IF
  DEALLOCATE(DA)

  CALL BIO_OPEN(P, TRIM(FIL)//'.LY', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(LY)'
  END IF
  CALL BIO_WRITE_D1(P, N, DLAMBDA, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D1(LY)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(LY)'
  END IF
  DEALLOCATE(DLAMBDA)

CONTAINS

  SUBROUTINE READCL(LAMBDA, SEED, N, FIL, IDIST, EPS, SCAL, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(OUT) :: SEED, N, IDIST, INFO
    DOUBLE PRECISION, INTENT(OUT) :: EPS, SCAL
    CHARACTER(LEN=*), INTENT(OUT) :: LAMBDA, FIL

    INTEGER, PARAMETER :: NRQP = 4
    INTEGER :: NXTA
    CHARACTER(LEN=FNL) :: CAS

    SEED = 0
    N = 0
    IDIST = 0
    INFO = 0

    EPS = ZERO
    SCAL = ZERO

    LAMBDA = ''
    FIL = ''

    NXTA = NRQP
    CAS = ''

    IF (COMMAND_ARGUMENT_COUNT() .LT. NRQP) THEN
       WRITE (*,*) 'dgenhsvd.exe LAMBDA SEEDIX N FILE [ LAMBDA_PARAMS ]'
       WRITE (*,*) '>> COMMAND LINE (INPUT) ARGUMENTS <<'
       WRITE (*,*) 'LAMBDA  : \Lambda(A); 1, 2, 3, or FILENAME'
       WRITE (*,*) 'IDIST123: 1 [uniform (0,1)], 2 [uniform(-1,1)], or 3 [normal(0,1)]'
       WRITE (*,*) 'FILENAME: LAMBDA.txt: max 256 chars, >= N lines [each line = one real value]'
       WRITE (*,*) 'SEEDIX  : index of hard-coded pRNG seed (see seedix.F90); 1 or 2'
       WRITE (*,*) 'N       : order of the output matrix: > 0'
       WRITE (*,*) 'FILE    : output file name prefix: max 256 chars'
       WRITE (*,*) 'LAMBDA  ; LAMBDA_PARAMS if LAMBDA is IDIST123'
       WRITE (*,*) ' EPS    : \lambda''_i survives iff |\lambda''_i| > EPS'
       WRITE (*,*) ' SCALE  : final \lambda_i = \lambda''_i * SCALE'
       WRITE (*,*) '<< OUTPUT DATASETS >>'
       WRITE (*,*) 'FILE.Y  : double precision(N,N); a factor F (A = F^T J F)'
       WRITE (*,*) 'FILE.J  : integer*8(N); diagonal of the sign matrix J'
       WRITE (*,*) 'FILE.LY : double precision(N); \Lambda(A) as read/generated'
       INFO = 1
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(1, LAMBDA, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -1
       RETURN
    END IF
    IF (TRIM(LAMBDA) .EQ. '1') THEN
       IDIST = 1
    ELSE IF (TRIM(LAMBDA) .EQ. '2') THEN
       IDIST = 2
    ELSE IF (TRIM(LAMBDA) .EQ. '3') THEN
       IDIST = 3
    END IF

    CALL GET_COMMAND_ARGUMENT(2, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -2
       RETURN
    END IF
    READ (CAS,*) SEED
    IF (SEED .LE. 0) THEN
       INFO = -2
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(3, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -3
       RETURN
    END IF
    READ (CAS,*) N
    IF (N .LE. 0) THEN
       INFO = -3
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(4, FIL, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -4
       RETURN
    END IF

    IF (IDIST .NE. 0) THEN
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
       IF (SCAL .EQ. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
    END IF
  END SUBROUTINE READCL

#include "bio.F90"
END PROGRAM DGENHSVD

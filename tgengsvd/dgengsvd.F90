PROGRAM DGENGSVD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ERROR_UNIT
  IMPLICIT NONE
#include "qx_wp.fi"

  INTEGER, PARAMETER :: FNL = 256
  INTEGER, PARAMETER :: IOMSGLEN = 66
  DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0

  ! command-line parameters
  INTEGER :: SEED, N, IDIST_F, IDIST_G, IDIST_X, INFO
  DOUBLE PRECISION :: EPS_F, SCAL_F, EPS_G, SCAL_G, EPS_X, SCAL_X
  CHARACTER(LEN=FNL) :: SIGMA_F, SIGMA_G, LAMBDA_X, FIL

  INTEGER :: ISEED(4), K, L, P, LWORK
  DOUBLE PRECISION :: TOLA, TOLB, WORK1(1), ULP, UNFL
  REAL(WP) :: H

  INTEGER, ALLOCATABLE :: IWORK(:)

  DOUBLE PRECISION, ALLOCATABLE :: DS_F(:), DS_G(:), DS(:), DL_X(:), TAU(:), DWORK(:)
  REAL(WP), ALLOCATABLE :: QS_F(:), QS_G(:), QL_X(:), QWORK(:)

  DOUBLE PRECISION, ALLOCATABLE :: DF(:,:), DG(:,:), DU(:,:), DV(:,:), DQ(:,:)
  REAL(WP), ALLOCATABLE :: QF(:,:), QG(:,:), QX(:,:)

  DOUBLE PRECISION, EXTERNAL :: DLAMCH, DLANGE
  EXTERNAL :: DGGSVP3, SEEDIX, DGENLAM, DTXTLAM, DGENDAT

  CALL READCL(SIGMA_F, SIGMA_G, LAMBDA_X, SEED, N, FIL, &
       IDIST_F, EPS_F, SCAL_F, IDIST_G, EPS_G, SCAL_G, IDIST_X, EPS_X, SCAL_X, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'READCL'
  END IF

  CALL SEEDIX(SEED, ISEED, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'SEEDIX'
  END IF

  ALLOCATE(DS_F(N))
  IF (IDIST_F .NE. 0) THEN
     CALL DGENLAM(N, ISEED, IDIST_F, EPS_F, SCAL_F, DS_F, P, INFO)
  ELSE
     CALL DTXTLAM(SIGMA_F, N, DS_F, P, INFO)
  END IF
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'SIGMA_F'
  END IF
  IF (P .NE. N) THEN
     WRITE (*,*) P, '<', N
     STOP 'SIGMA_F'
  END IF

  ALLOCATE(DS_G(N))
  IF (IDIST_G .NE. 0) THEN
     CALL DGENLAM(N, ISEED, IDIST_G, EPS_G, SCAL_G, DS_G, P, INFO)
  ELSE
     CALL DTXTLAM(SIGMA_G, N, DS_G, P, INFO)
  END IF
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'SIGMA_G'
  END IF
  IF (P .NE. N) THEN
     WRITE (*,*) P, '<', N
     STOP 'SIGMA_G'
  END IF

  ALLOCATE(DS(N))
  DO P = 1, N
     DS(P) = DS_F(P) / DS_G(P)
  END DO

  ALLOCATE(DL_X(N))
  IF (IDIST_X .NE. 0) THEN
     CALL DGENLAM(N, ISEED, IDIST_X, EPS_X, SCAL_X, DL_X, P, INFO)
  ELSE
     CALL DTXTLAM(LAMBDA_X, N, DL_X, P, INFO)
  END IF
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'LAMBDA_X'
  END IF

  ALLOCATE(DF(N,N))
  ALLOCATE(DG(N,N))
  ALLOCATE(DU(N,N))

  ALLOCATE(QF(N,N))
  ALLOCATE(QG(N,N))
  ALLOCATE(QX(N,N))

  ALLOCATE(QS_F(N))
  ALLOCATE(QS_G(N))
  ALLOCATE(QL_X(N))

  DO P = 1, N
     QS_F(P) = REAL(DS_F(P),WP)
     QS_G(P) = REAL(DS_G(P),WP)
     QL_X(P) = REAL(DL_X(P),WP)

     H = HYPOT(QS_F(P), QS_G(P))
     QS_F(P) = QS_F(P) / H
     QS_G(P) = QS_G(P) / H

     DS_F(P) = DBLE(QS_F(P))
     DS_G(P) = DBLE(QS_G(P))
  END DO

  P = 3 * N
  ALLOCATE(QWORK(P))
  CALL DGENDAT(N, ISEED, QS_F, QS_G, QL_X, QF, DF, QG, DG, QX, DU, QWORK, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'DGENDAT'
  END IF
  DEALLOCATE(QWORK)

  DEALLOCATE(QL_X)
  DEALLOCATE(QS_G)
  DEALLOCATE(QS_F)

  DEALLOCATE(QX)
  DEALLOCATE(QG)
  DEALLOCATE(QF)

  ALLOCATE(DV(N,N))
  ALLOCATE(DQ(N,N))

  ALLOCATE(IWORK(N))
  ALLOCATE(TAU(N))

  TOLA = ZERO
  TOLB = ZERO
  WORK1 = ZERO
  LWORK = -1
  CALL DGGSVP3('U', 'V', 'Q', N, N, N, DF,N, DG,N, TOLA,TOLB, K, L, DU,N, DV,N, DQ,N, IWORK, TAU, WORK1, LWORK, INFO)
  LWORK = MAX(1,CEILING(WORK1(1)))
  ALLOCATE(DWORK(LWORK))

  TOLA = DLANGE('1', N, N, DF, N, DWORK) ! M=N x N
  TOLB = DLANGE('1', N, N, DG, N, DWORK) ! P=N x N
  ULP = DLAMCH('P') ! Precision
  UNFL = DLAMCH('S') ! Safe Minimum
  TOLA = N * MAX(TOLA,UNFL) * ULP ! N=MAX(M,N)
  TOLB = N * MAX(TOLB,UNFL) * ULP ! N=MAX(P,N)

  CALL DGGSVP3('U', 'V', 'Q', N, N, N, DF,N, DG,N, TOLA,TOLB, K, L, DU,N, DV,N, DQ,N, IWORK, TAU, DWORK, LWORK, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'DGGSVP3'
  END IF

  DEALLOCATE(DWORK)
  DEALLOCATE(TAU)
  DEALLOCATE(IWORK)

  CALL BIO_OPEN(P, TRIM(FIL)//'.QQ', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(QQ)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DQ, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(QQ)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(QQ)'
  END IF
  DEALLOCATE(DQ)

  CALL BIO_OPEN(P, TRIM(FIL)//'.QV', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(QV)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DV, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(QV)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(QV)'
  END IF
  DEALLOCATE(DV)

  CALL BIO_OPEN(P, TRIM(FIL)//'.QU', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(QU)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DU, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(QU)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(QU)'
  END IF
  DEALLOCATE(DU)

  CALL BIO_OPEN(P, TRIM(FIL)//'.W', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(W)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DG, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(W)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(W)'
  END IF
  DEALLOCATE(DG)

  CALL BIO_OPEN(P, TRIM(FIL)//'.Y', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(Y)'
  END IF
  CALL BIO_WRITE_D2(P, N, N, DF, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D2(Y)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(Y)'
  END IF
  DEALLOCATE(DF)

  CALL BIO_OPEN(P, TRIM(FIL)//'.LX', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(LX)'
  END IF
  CALL BIO_WRITE_D1(P, N, DL_X, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D1(LX)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(LX)'
  END IF
  DEALLOCATE(DL_X)

  CALL BIO_OPEN(P, TRIM(FIL)//'.SS', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(SS)'
  END IF
  CALL BIO_WRITE_D1(P, N, DS, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D1(SS)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(SS)'
  END IF
  DEALLOCATE(DS)

  CALL BIO_OPEN(P, TRIM(FIL)//'.SW', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(SW)'
  END IF
  CALL BIO_WRITE_D1(P, N, DS_G, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D1(SW)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(SW)'
  END IF
  DEALLOCATE(DS_G)

  CALL BIO_OPEN(P, TRIM(FIL)//'.SY', 'WO', INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_OPEN(SY)'
  END IF
  CALL BIO_WRITE_D1(P, N, DS_F, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_WRITE_D1(SY)'
  END IF
  CALL BIO_CLOSE(P, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (*,*) INFO
     STOP 'BIO_CLOSE(SY)'
  END IF
  DEALLOCATE(DS_F)

CONTAINS

  SUBROUTINE READCL(SIGMA_F, SIGMA_G, LAMBDA_X, SEED, N, FIL, &
       IDIST_F, EPS_F, SCAL_F, IDIST_G, EPS_G, SCAL_G, IDIST_X, EPS_X, SCAL_X, INFO)
    IMPLICIT NONE

    INTEGER, INTENT(OUT) :: SEED, N, IDIST_F, IDIST_G, IDIST_X, INFO
    DOUBLE PRECISION, INTENT(OUT) :: EPS_F, SCAL_F, EPS_G, SCAL_G, EPS_X, SCAL_X
    CHARACTER(LEN=*), INTENT(OUT) :: SIGMA_F, SIGMA_G, LAMBDA_X, FIL

    INTEGER, PARAMETER :: NRQP = 6
    INTEGER :: NXTA
    CHARACTER(LEN=FNL) :: CAS

    SEED = 0
    N = 0
    IDIST_F = 0
    IDIST_G = 0
    IDIST_X = 0
    INFO = 0

    EPS_F = ZERO
    SCAL_F = ZERO
    EPS_G = ZERO
    SCAL_G = ZERO
    EPS_X = ZERO
    SCAL_X = ZERO

    SIGMA_F = ''
    SIGMA_G = ''
    LAMBDA_X = ''
    FIL = ''

    NXTA = NRQP
    CAS = ''

    IF (COMMAND_ARGUMENT_COUNT() .LT. NRQP) THEN
       WRITE (*,*) 'dgengsvd.exe SIGMA_F SIGMA_G LAMBDA_X SEEDIX N FILE [ SIG|LAM_PARAMS ]'
       WRITE (*,*) '>> COMMAND LINE (INPUT) ARGUMENTS <<'
       WRITE (*,*) 'SIGMA_F : \Sigma(F); 1, 3, or FILENAME'
       WRITE (*,*) 'SIGMA_G : \Sigma(G); 1, 3, or FILENAME'
       WRITE (*,*) 'LAMBDA_X: \Lambda(X); 1, 2, 3, or FILENAME'
       WRITE (*,*) 'IDIST123: 1 [uniform (0,1)], 2 [uniform(-1,1)], or 3 [normal(0,1)]'
       WRITE (*,*) 'FILENAME: SIG|LAM.txt: max 256 chars, >= N lines [each line = one real value]'
       WRITE (*,*) 'SEEDIX  : index of hard-coded pRNG seed (see seedix.F90); 1 or 2'
       WRITE (*,*) 'N       : order of the output matrix: > 0'
       WRITE (*,*) 'FILE    : output file name prefix: max 256 chars'
       WRITE (*,*) 'SIG|LAM ; SIG|LAM_PARAMS if SIG|LAM is IDIST123'
       WRITE (*,*) ' EPS_F  : \sigma''_i survives iff |\sigma''_i| > EPS_F'
       WRITE (*,*) ' SCALE_F: final \sigma_i = \sigma''_i * SCALE_F'
       WRITE (*,*) ' EPS_G  : \sigma''_i survives iff |\sigma''_i| > EPS_G'
       WRITE (*,*) ' SCALE_G: final \sigma_i = \sigma''_i * SCALE_G'
       WRITE (*,*) ' EPS_X  : \lambda''_i survives iff |\lambda''_i| > EPS_X'
       WRITE (*,*) ' SCALE_X: final \lambda_i = \lambda''_i * SCALE_X'
       WRITE (*,*) '<< OUTPUT DATASETS >>'
       WRITE (*,*) 'FILE.SY : double precision(N); \Sigma(F) normalized: \sigma_F^2 + \sigma_G^2 = 1'
       WRITE (*,*) 'FILE.SW : double precision(N); \Sigma(G) normalized: \sigma_F^2 + \sigma_G^2 = 1'
       WRITE (*,*) 'FILE.SS : double precision(N); \Sigma: \sigma_F / \sigma_G'
       WRITE (*,*) 'FILE.LX : double precision(N); \Lambda(X) as read/generated'
       WRITE (*,*) 'FILE.Y  : double precision(N,N); F = U_F \Sigma(F) X'
       WRITE (*,*) 'FILE.W  : double precision(N,N); G = U_G \Sigma(G) X'
       WRITE (*,*) 'FILE.QU : double precision(N,N); U: see LaPACK dggsvp3.f'
       WRITE (*,*) 'FILE.QV : double precision(N,N); V: see LaPACK dggsvp3.f'
       WRITE (*,*) 'FILE.QQ : double precision(N,N); Q: see LaPACK dggsvp3.f'
       INFO = 1
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(1, SIGMA_F, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -1
       RETURN
    END IF
    IF (TRIM(SIGMA_F) .EQ. '1') THEN
       IDIST_F = 1
    ELSE IF (TRIM(SIGMA_F) .EQ. '3') THEN
       IDIST_F = 3
    END IF

    CALL GET_COMMAND_ARGUMENT(2, SIGMA_G, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -2
       RETURN
    END IF
    IF (TRIM(SIGMA_G) .EQ. '1') THEN
       IDIST_G = 1
    ELSE IF (TRIM(SIGMA_G) .EQ. '3') THEN
       IDIST_G = 3
    END IF

    CALL GET_COMMAND_ARGUMENT(3, LAMBDA_X, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -3
       RETURN
    END IF
    IF (TRIM(LAMBDA_X) .EQ. '1') THEN
       IDIST_X = 1
    ELSE IF (TRIM(LAMBDA_X) .EQ. '2') THEN
       IDIST_X = 2
    ELSE IF (TRIM(LAMBDA_X) .EQ. '3') THEN
       IDIST_X = 3
    END IF

    CALL GET_COMMAND_ARGUMENT(4, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -4
       RETURN
    END IF
    READ (CAS,*) SEED
    IF (SEED .LE. 0) THEN
       INFO = -4
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(5, CAS, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -5
       RETURN
    END IF
    READ (CAS,*) N
    IF (N .LE. 0) THEN
       INFO = -5
       RETURN
    END IF

    CALL GET_COMMAND_ARGUMENT(6, FIL, STATUS=INFO)
    IF (INFO .NE. 0) THEN
       INFO = -6
       RETURN
    END IF

    IF (IDIST_F .NE. 0) THEN
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) EPS_F
       IF (EPS_F .LT. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) SCAL_F
       IF (SCAL_F .EQ. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
    END IF

    IF (IDIST_G .NE. 0) THEN
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) EPS_G
       IF (EPS_G .LT. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) SCAL_G
       IF (SCAL_G .EQ. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
    END IF

    IF (IDIST_X .NE. 0) THEN
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) EPS_X
       IF (EPS_X .LT. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
       NXTA = NXTA + 1
       CALL GET_COMMAND_ARGUMENT(NXTA, CAS, STATUS=INFO)
       IF (INFO .NE. 0) THEN
          INFO = -NXTA
          RETURN
       END IF
       READ (CAS,*) SCAL_X
       IF (SCAL_X .EQ. ZERO) THEN
          INFO = -NXTA
          RETURN
       END IF
    END IF
  END SUBROUTINE READCL

#include "bio.F90"
END PROGRAM DGENGSVD

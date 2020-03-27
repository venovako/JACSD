MODULE BINIO
  USE, INTRINSIC :: ISO_C_BINDING
  USE VN_BINIO_F
  IMPLICIT NONE

CONTAINS
  ! _RO: read-only, _RW: read-write, _WO: overwrite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_RO(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(OUT) :: SZ, FD

    INTEGER(KIND=c_size_t) :: C_SZ

    FD = INT(VN_BOPEN_RO((TRIM(FN)//c_null_char), C_SZ))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_RO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_RW(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(INOUT) :: SZ
    INTEGER, INTENT(OUT) :: FD

    INTEGER(KIND=c_size_t) :: C_SZ

    C_SZ = INT(SZ, c_size_t)
    FD = INT(VN_BOPEN_RW((TRIM(FN)//c_null_char), C_SZ))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_RW

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_WO(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(OUT) :: SZ, FD

    INTEGER(KIND=c_size_t) :: C_SZ

    C_SZ = -1_c_size_t
    FD = INT(VN_BOPEN_WO((TRIM(FN)//c_null_char), C_SZ))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_WO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  INTEGER FUNCTION BWRITE(FD, BUF, nB, OFF)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FD, nB, OFF
    TYPE(c_ptr), INTENT(IN) :: BUF
    BWRITE = INT(VN_BWRITE(INT(FD,c_int), BUF, INT(nB,c_size_t), INT(OFF,c_size_t)))
  END FUNCTION BWRITE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  INTEGER FUNCTION BREAD(FD, BUF, nB, OFF)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FD, nB, OFF
    TYPE(c_ptr), INTENT(IN) :: BUF
    BREAD = INT(VN_BREAD(INT(FD,c_int), BUF, INT(nB,c_size_t), INT(OFF,c_size_t)))
  END FUNCTION BREAD

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BCLOSE(FD)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: FD
    IF (FD .GE. 0) FD = INT(VN_BCLOSE(INT(FD,c_int)))
  END SUBROUTINE BCLOSE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BCLOSEN(FD, N)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(INOUT) :: FD(N)

    INTEGER :: I

    DO I = N, 1, -1
       CALL BCLOSE(FD(I))
    END DO
  END SUBROUTINE BCLOSEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE BINIO

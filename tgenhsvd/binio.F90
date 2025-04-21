MODULE BINIO
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTERFACE
#ifdef _WIN32
     FUNCTION BREADD(FD, BUF, SZ, OFF) BIND(C,NAME='PVN_BREAD')
#else
     FUNCTION BREADD(FD, BUF, SZ, OFF) BIND(C,NAME='pvn_bread_')
#endif
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       INTEGER(KIND=c_int), INTENT(IN) :: FD
       REAL(KIND=c_double), INTENT(OUT) :: BUF(*)
       INTEGER(KIND=c_size_t), INTENT(IN) :: SZ, OFF
       INTEGER(KIND=c_size_t) :: BREADD
     END FUNCTION BREADD
  END INTERFACE

  INTERFACE
#ifdef _WIN32
     FUNCTION BREADZ(FD, BUF, SZ, OFF) BIND(C,NAME='PVN_BREAD')
#else
     FUNCTION BREADZ(FD, BUF, SZ, OFF) BIND(C,NAME='pvn_bread_')
#endif
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       INTEGER(KIND=c_int), INTENT(IN) :: FD
       COMPLEX(KIND=c_double), INTENT(OUT) :: BUF(*)
       INTEGER(KIND=c_size_t), INTENT(IN) :: SZ, OFF
       INTEGER(KIND=c_size_t) :: BREADZ
     END FUNCTION BREADZ
  END INTERFACE

CONTAINS
  ! _RO: read-only, _RW: read-write, _WO: overwrite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_RO(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(OUT) :: SZ, FD

    INTEGER(KIND=c_size_t) :: C_SZ
    INTEGER(KIND=c_int), EXTERNAL :: PVN_BOPEN_RO

    FD = INT(PVN_BOPEN_RO(C_SZ, (TRIM(FN)//c_null_char)))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_RO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_RW(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(INOUT) :: SZ
    INTEGER, INTENT(OUT) :: FD

    INTEGER(KIND=c_size_t) :: C_SZ
    INTEGER(KIND=c_int), EXTERNAL :: PVN_BOPEN_RW

    C_SZ = INT(SZ, c_size_t)
    FD = INT(PVN_BOPEN_RW(C_SZ, (TRIM(FN)//c_null_char)))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_RW

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BOPEN_WO(FN, SZ, FD)
    IMPLICIT NONE
    CHARACTER(LEN=*,KIND=c_char), INTENT(IN) :: FN
    INTEGER, INTENT(OUT) :: SZ, FD

    INTEGER(KIND=c_size_t) :: C_SZ
    INTEGER(KIND=c_int), EXTERNAL :: PVN_BOPEN_WO

    C_SZ = -1_c_size_t
    FD = INT(PVN_BOPEN_WO(C_SZ, (TRIM(FN)//c_null_char)))
    SZ = INT(C_SZ)
  END SUBROUTINE BOPEN_WO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE BCLOSE(FD)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: FD
    INTEGER(KIND=c_int), EXTERNAL :: PVN_BCLOSE
    FD = INT(PVN_BCLOSE(INT(FD,c_int)))
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

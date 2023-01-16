MODULE VN_AUX_F
  IMPLICIT NONE
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE INTEGER FUNCTION GCD(A, B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A, B
    INTEGER :: AA, BB, CC

    AA = ABS(A)
    BB = ABS(B)
    IF (AA .LT. BB) THEN
       CC = AA
       AA = BB
       BB = CC
    END IF

    DO WHILE (BB .GT. 0)
       AA = AA - BB
       IF (AA .LT. BB) THEN
          CC = AA
          AA = BB
          BB = CC
       END IF
    END DO

    GCD = AA
  END FUNCTION GCD

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE INTEGER FUNCTION LCM(A, B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A, B
    INTEGER :: G

    G = GCD(A, B)
    IF (G .EQ. 0) THEN
       LCM = 0
    ELSE
       LCM = (ABS(A) / G) * ABS(B)
    END IF
  END FUNCTION LCM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE STHALT(MSG)
#ifdef __INTEL_COMPILER
#ifdef VN_USE_C_BACKTRACE
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#else
    USE IFCORE, ONLY: TRACEBACKQQ
#endif
#else
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#ifndef __GFORTRAN__
#ifndef VN_USE_C_BACKTRACE
#define VN_USE_C_BACKTRACE
#endif
#endif
#endif
#ifdef VN_USE_C_BACKTRACE
    USE, INTRINSIC :: ISO_C_BINDING
#endif
#ifdef _OPENMP
    USE OMP_LIB, ONLY: OMP_GET_THREAD_NUM
#endif
    IMPLICIT NONE
#ifdef VN_USE_C_BACKTRACE
    INTERFACE
       FUNCTION BTRACE(ARR, SIZ) BIND(C,NAME='backtrace')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_ptr
         IMPLICIT NONE
         INTEGER(KIND=c_int), INTENT(IN), VALUE :: SIZ
         TYPE(c_ptr), INTENT(OUT), TARGET :: ARR(SIZ)
         INTEGER(KIND=c_int) :: BTRACE
       END FUNCTION BTRACE
    END INTERFACE
    INTERFACE
       FUNCTION BTSYM(ARR, SIZ) BIND(C,NAME='backtrace_symbols')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_ptr
         IMPLICIT NONE
         INTEGER(KIND=c_int), INTENT(IN), VALUE :: SIZ
         TYPE(c_ptr), INTENT(IN), TARGET :: ARR(SIZ)
         TYPE(c_ptr) :: BTSYM
       END FUNCTION BTSYM
    END INTERFACE
    INTERFACE
       FUNCTION CALEN(S) BIND(C,NAME='strlen')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_size_t, c_ptr
         TYPE(c_ptr), INTENT(IN), VALUE :: S
         INTEGER(KIND=c_size_t) :: CALEN
       END FUNCTION CALEN
    END INTERFACE
    INTERFACE
       SUBROUTINE CFREE(P) BIND(C,NAME='free')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_ptr
         IMPLICIT NONE
         TYPE(c_ptr), INTENT(IN), VALUE :: P
       END SUBROUTINE CFREE
    END INTERFACE
    INTEGER(KIND=c_int), PARAMETER :: BTSIZ = 128_c_int
#endif
    CHARACTER(LEN=*), INTENT(IN) :: MSG
    CHARACTER(LEN=6) :: THR
#ifdef VN_USE_C_BACKTRACE
    TYPE(c_ptr) :: BTARR(BTSIZ), SYMA
    TYPE(c_ptr), POINTER :: CA(:)
    CHARACTER(KIND=c_char), POINTER :: FA(:)
    INTEGER(KIND=c_int) :: ASIZ, I, J, K
#endif
#ifdef _OPENMP
    WRITE (THR,'(A,I3,A)') '[', OMP_GET_THREAD_NUM(), '] '
#else
#ifdef NDEBUG
    THR = '[!!!] '
#else
    THR = '[???] '
#endif
#endif
#ifdef VN_USE_C_BACKTRACE
    WRITE (ERROR_UNIT,'(A)') NEW_LINE(MSG)//THR//TRIM(MSG)
    FLUSH(ERROR_UNIT)
    ASIZ = BTRACE(BTARR, BTSIZ)
    IF (ASIZ .GT. 0) THEN
       SYMA = BTSYM(BTARR, ASIZ)
       CALL C_F_POINTER(SYMA, CA, [ASIZ])
       DO I = 1_c_int, ASIZ
          K = INT(CALEN(CA(I)))
          CALL C_F_POINTER(CA(I), FA, [K])
          DO J = 1, K-1
             WRITE (ERROR_UNIT,'(A)',ADVANCE='NO') FA(J)
          END DO
          WRITE (ERROR_UNIT,'(A)') FA(K)
          FA => NULL()
       END DO
       FLUSH(ERROR_UNIT)
       CALL CFREE(SYMA)
    END IF
    CA => NULL()
#else
#ifdef __GFORTRAN__
    WRITE (ERROR_UNIT,'(A)') NEW_LINE(MSG)//THR//TRIM(MSG)
    FLUSH(ERROR_UNIT)
    CALL BACKTRACE
#else
    CALL TRACEBACKQQ(NEW_LINE(MSG)//THR//TRIM(MSG), -1_4)
#endif
#endif
    STOP
  END SUBROUTINE STHALT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE VN_AUX_F

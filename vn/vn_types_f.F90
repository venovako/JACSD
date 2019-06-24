MODULE VN_TYPES_F
#ifndef NDEBUG
  USE, INTRINSIC :: IEEE_ARITHMETIC
  USE, INTRINSIC :: IEEE_FEATURES
#endif
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTEGER(KIND=c_int32_t), PARAMETER :: S_QNAN_MASK = 2143289344_c_int32_t
  INTEGER(KIND=c_int64_t), PARAMETER :: D_QNAN_MASK = 9221120237041090560_c_int64_t

  INTERFACE QUIET_NAN
     MODULE PROCEDURE S_QUIET_NAN
     MODULE PROCEDURE D_QUIET_NAN
  END INTERFACE QUIET_NAN

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION S_QUIET_NAN(PAYLOAD)
    IMPLICIT NONE
    INTEGER(KIND=c_int), INTENT(IN) :: PAYLOAD
    REAL(KIND=c_float) :: S_QUIET_NAN
    S_QUIET_NAN = TRANSFER(IOR(PAYLOAD, S_QNAN_MASK), 0.0_c_float)
  END FUNCTION S_QUIET_NAN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION D_QUIET_NAN(PAYLOAD)
    IMPLICIT NONE
    INTEGER(KIND=c_int64_t), INTENT(IN) :: PAYLOAD
    REAL(KIND=c_double) :: D_QUIET_NAN
    D_QUIET_NAN = TRANSFER(IOR(PAYLOAD, D_QNAN_MASK), 0.0_c_double)
  END FUNCTION D_QUIET_NAN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION S_VERIFY_MIN(STRONG)
#else
  LOGICAL FUNCTION S_VERIFY_MIN(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
#ifndef NDEBUG
    LOGICAL(c_int) :: HM_INV
    CALL IEEE_GET_HALTING_MODE(IEEE_INVALID, HM_INV)
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, .FALSE._c_int)
#endif
    IF (STRONG) THEN
       S_VERIFY_MIN = (&
            (MIN(S_QUIET_NAN(-1_c_int32_t), 0.0_c_float) .EQ. 0.0_c_float) .AND. &
            (MIN(0.0_c_float, S_QUIET_NAN(-1_c_int32_t)) .EQ. 0.0_c_float))
    ELSE ! weak
       S_VERIFY_MIN = (MIN(S_QUIET_NAN(-1_c_int32_t), 0.0_c_float) .EQ. 0.0_c_float)
    END IF
#ifndef NDEBUG
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, HM_INV)
#endif
  END FUNCTION S_VERIFY_MIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION D_VERIFY_MIN(STRONG)
#else
  LOGICAL FUNCTION D_VERIFY_MIN(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
#ifndef NDEBUG
    LOGICAL(c_int) :: HM_INV
    CALL IEEE_GET_HALTING_MODE(IEEE_INVALID, HM_INV)
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, .FALSE._c_int)
#endif
    IF (STRONG) THEN
       D_VERIFY_MIN = (&
            (MIN(D_QUIET_NAN(-1_c_int64_t), 0.0_c_double) .EQ. 0.0_c_double) .AND. &
            (MIN(0.0_c_double, D_QUIET_NAN(-1_c_int64_t)) .EQ. 0.0_c_double))
    ELSE ! weak
       D_VERIFY_MIN = (MIN(D_QUIET_NAN(-1_c_int64_t), 0.0_c_double) .EQ. 0.0_c_double)
    END IF
#ifndef NDEBUG
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, HM_INV)
#endif
  END FUNCTION D_VERIFY_MIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION VERIFY_MIN(STRONG)
#else
  LOGICAL FUNCTION VERIFY_MIN(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
    VERIFY_MIN = (S_VERIFY_MIN(STRONG) .AND. D_VERIFY_MIN(STRONG))
  END FUNCTION VERIFY_MIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION S_VERIFY_MAX(STRONG)
#else
  LOGICAL FUNCTION S_VERIFY_MAX(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
#ifndef NDEBUG
    LOGICAL(c_int) :: HM_INV
    CALL IEEE_GET_HALTING_MODE(IEEE_INVALID, HM_INV)
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, .FALSE._c_int)
#endif
    IF (STRONG) THEN
       S_VERIFY_MAX = (&
            (MAX(S_QUIET_NAN(0_c_int32_t), -1.0_c_float) .EQ. -1.0_c_float) .AND. &
            (MAX(-1.0_c_float, S_QUIET_NAN(0_c_int32_t)) .EQ. -1.0_c_float))
    ELSE ! weak
       S_VERIFY_MAX = (MAX(S_QUIET_NAN(0_c_int32_t), -1.0_c_float) .EQ. -1.0_c_float)
    END IF
#ifndef NDEBUG
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, HM_INV)
#endif
  END FUNCTION S_VERIFY_MAX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION D_VERIFY_MAX(STRONG)
#else
  LOGICAL FUNCTION D_VERIFY_MAX(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
#ifndef NDEBUG
    LOGICAL(c_int) :: HM_INV
    CALL IEEE_GET_HALTING_MODE(IEEE_INVALID, HM_INV)
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, .FALSE._c_int)
#endif
    IF (STRONG) THEN
       D_VERIFY_MAX = (&
            (MAX(D_QUIET_NAN(0_c_int64_t), -1.0_c_double) .EQ. -1.0_c_double) .AND. &
            (MAX(-1.0_c_double, D_QUIET_NAN(0_c_int64_t)) .EQ. -1.0_c_double))
    ELSE ! weak
       D_VERIFY_MAX = (MAX(D_QUIET_NAN(0_c_int64_t), -1.0_c_double) .EQ. -1.0_c_double)
    END IF
#ifndef NDEBUG
    CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, HM_INV)
#endif
  END FUNCTION D_VERIFY_MAX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION VERIFY_MAX(STRONG)
#else
  LOGICAL FUNCTION VERIFY_MAX(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
    VERIFY_MAX = (S_VERIFY_MAX(STRONG) .AND. D_VERIFY_MAX(STRONG))
  END FUNCTION VERIFY_MAX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef NDEBUG
  PURE LOGICAL FUNCTION VERIFY_MIN_MAX(STRONG)
#else
  LOGICAL FUNCTION VERIFY_MIN_MAX(STRONG)
#endif
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: STRONG
    VERIFY_MIN_MAX = (VERIFY_MIN(STRONG) .AND. VERIFY_MAX(STRONG))
  END FUNCTION VERIFY_MIN_MAX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE VN_TYPES_F

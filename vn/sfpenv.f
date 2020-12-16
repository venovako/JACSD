      SUBROUTINE SFPENV(ROUNDM, MODES, FPSTAT, INFO)
      USE, INTRINSIC :: IEEE_ARITHMETIC
      IMPLICIT NONE
!
!     PARAMETERS
!
      REAL, PARAMETER :: ZERO = 0.0E0
!     halting: overflow, divide by zero, invalid, underflow, inexact
      INTEGER, PARAMETER :: OF = 1, DZ = 2, IV = 3, UF = 4, IE = 5
!     gradual underflow; length of MODES
      INTEGER, PARAMETER :: GU = 6, MXMODE = GU
!
!     INPUT ARGUMENTS
!
      TYPE(IEEE_ROUND_TYPE), INTENT(IN) :: ROUNDM
      LOGICAL, INTENT(IN) :: MODES(MXMODE)
!
!     OUTPUT ARGUMENTS
!
      TYPE(IEEE_STATUS_TYPE), INTENT(OUT) :: FPSTAT
      INTEGER, INTENT(OUT) :: INFO
!
!     LOCAL VARIABLES
!
      TYPE(IEEE_ROUND_TYPE) :: R
      LOGICAL :: L
!
!     EXECUTABLE STATEMENTS
!
      CALL IEEE_GET_STATUS(FPSTAT)
      IF (IEEE_SUPPORT_DATATYPE(ZERO)) THEN
         INFO = 0
      ELSE
!     REAL datatype not supported
         INFO = -3
         RETURN
      END IF
!
      CALL IEEE_GET_ROUNDING_MODE(R)
      IF (ROUNDM .NE. R) THEN
         IF (IEEE_SUPPORT_ROUNDING(ROUNDM, ZERO)) THEN
            CALL IEEE_SET_ROUNDING_MODE(ROUNDM)
         ELSE
!     ROUNDM rounding mode not supported
            INFO = -1
            RETURN
         END IF
      END IF
!
      CALL IEEE_GET_HALTING_MODE(IEEE_OVERFLOW, L)
      IF (MODES(OF) .NEQV. L) THEN
         IF (IEEE_SUPPORT_HALTING(IEEE_OVERFLOW)) THEN
            CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW, MODES(OF))
         ELSE
!     cannot change the halting mode
            INFO = OF
            RETURN
         END IF
      END IF
      CALL IEEE_GET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO, L)
      IF (MODES(DZ) .NEQV. L) THEN
         IF (IEEE_SUPPORT_HALTING(IEEE_DIVIDE_BY_ZERO)) THEN
            CALL IEEE_SET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO, MODES(DZ))
         ELSE
!     cannot change the halting mode
            INFO = DZ
            RETURN
         END IF
      END IF
      CALL IEEE_GET_HALTING_MODE(IEEE_INVALID, L)
      IF (MODES(IV) .NEQV. L) THEN
         IF (IEEE_SUPPORT_HALTING(IEEE_INVALID)) THEN
            CALL IEEE_SET_HALTING_MODE(IEEE_INVALID, MODES(IV))
         ELSE
!     cannot change the halting mode
            INFO = IV
            RETURN
         END IF
      END IF
      CALL IEEE_GET_HALTING_MODE(IEEE_UNDERFLOW, L)
      IF (MODES(UF) .NEQV. L) THEN
         IF (IEEE_SUPPORT_HALTING(IEEE_UNDERFLOW)) THEN
            CALL IEEE_SET_HALTING_MODE(IEEE_UNDERFLOW, MODES(UF))
         ELSE
!     cannot change the halting mode
            INFO = UF
            RETURN
         END IF
      END IF
      CALL IEEE_GET_HALTING_MODE(IEEE_INEXACT, L)
      IF (MODES(IE) .NEQV. L) THEN
         IF (IEEE_SUPPORT_HALTING(IEEE_INEXACT)) THEN
            CALL IEEE_SET_HALTING_MODE(IEEE_INEXACT, MODES(IE))
         ELSE
!     cannot change the halting mode
            INFO = IE
            RETURN
         END IF
      END IF
!
      CALL IEEE_GET_UNDERFLOW_MODE(L)
      IF (MODES(GU) .NEQV. L) THEN
         IF (IEEE_SUPPORT_UNDERFLOW_CONTROL(ZERO)) THEN
            CALL IEEE_SET_UNDERFLOW_MODE(MODES(GU))
         ELSE
!     cannot change the underflow mode
            INFO = GU
            RETURN
         END IF
      END IF
!     a few sanity checks for subnormals
      IF (MODES(GU)) THEN
         IF (.NOT. IEEE_SUPPORT_SUBNORMAL(ZERO)) THEN
            INFO = -4
            RETURN
         END IF
         IF (IEEE_VALUE(ZERO, IEEE_POSITIVE_SUBNORMAL) .EQ. ZERO) THEN
            INFO = -2
            RETURN
         END IF
      END IF
!
      END SUBROUTINE SFPENV

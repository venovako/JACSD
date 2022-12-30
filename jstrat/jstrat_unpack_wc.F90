PURE SUBROUTINE JSTRAT_UNPACK_WC(JS, ARR, PAIR, COMM, INFO)
#ifndef JSTRAT_F_MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32, INT64
#endif
  IMPLICIT NONE
#ifndef JSTRAT_F_MOD
  INTEGER(KIND=INT64), PARAMETER :: JSMLEN = 6_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSMEWC = 3_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSMMWC = 5_INT64
#endif
  INTEGER(KIND=INT64), INTENT(IN) :: JS(JSMLEN), ARR(2,2,*)
  INTEGER(KIND=INT64), INTENT(OUT) :: PAIR(2,*), COMM(2,*)
  INTEGER(KIND=INT64), INTENT(INOUT) :: INFO

  INTEGER(KIND=INT64) :: K, L
  INTEGER(KIND=INT32) :: C(2)
  EQUIVALENCE (L,C)

  SELECT CASE (JS(1))
  CASE (JSMEWC)
     IF (INFO .NE. (JS(2) / 2)) THEN
        INFO = -2
     ELSE
        DO K = 1, INFO
           PAIR(1,K) = ARR(1,1,K) + 1
           PAIR(2,K) = ARR(2,1,K) + 1
           COMM(1,K) = ARR(1,2,K)
           COMM(2,K) = ARR(2,2,K)
        END DO
     END IF
  CASE (JSMMWC)
     IF (INFO .NE. -(JS(2) / 2)) THEN
        INFO = -2
     ELSE
        INFO = -INFO
        DO K = 1, INFO
           L = ARR(1,1,K)
           PAIR(1,K) = C(1) + 1
           L = ARR(2,1,K)
           PAIR(2,K) = C(1) + 1
           L = ARR(1,2,K)
           COMM(1,K) = C(1)
           L = ARR(2,2,K)
           COMM(2,K) = C(1)
        END DO
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE JSTRAT_UNPACK_WC

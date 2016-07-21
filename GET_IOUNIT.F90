! Assumes at max one I/O unit per thread open at any time.
INTEGER FUNCTION GET_IOUNIT(TYP)

  IMPLICIT NONE

  CHARACTER, INTENT(IN) :: TYP

  SELECT CASE (TYP)
  CASE ('E', 'e') ! STDERR
     GET_IOUNIT = 0
  CASE ('I', 'i') ! STDIN
     GET_IOUNIT = 5
  CASE ('O', 'o') ! STDOUT
     GET_IOUNIT = 6
  CASE ('N', 'n')
     IF (OMP_GET_NUM_THREADS() .LE. 4) THEN
        GET_IOUNIT = 1 + OMP_GET_THREAD_NUM()
     ELSE
        GET_IOUNIT = 7 + OMP_GET_THREAD_NUM()
     END IF
  CASE DEFAULT
     GET_IOUNIT = -1
  END SELECT

END FUNCTION GET_IOUNIT

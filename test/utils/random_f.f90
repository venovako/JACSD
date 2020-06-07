PROGRAM RANDOM_F
  IMPLICIT NONE
  INTEGER :: N, I
  INTEGER, ALLOCATABLE :: SEED(:)

  CALL RANDOM_INIT(.TRUE., .TRUE.)
  CALL RANDOM_SEED(SIZE=N)
  WRITE (*,'(A,I2)') 'SIZE=', N
  ALLOCATE(SEED(N)); SEED = 0
  CALL RANDOM_SEED(GET=SEED)
  DO I = 1, N
     WRITE (*,'(A,I2,A,I11)') 'SEED(', I, ')=', SEED(I)
  END DO
  DEALLOCATE(SEED)
END PROGRAM RANDOM_F

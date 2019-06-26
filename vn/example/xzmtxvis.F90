! Build and run: please, see `xzmtxvis.sh`.
PROGRAM XZMTXVIS
  USE VN_CMPLXVIS_F
  IMPLICIT NONE

  ! File name prefix of length at most 7 for the dump, palette, and bitmap files.
  CHARACTER(LEN=7), PARAMETER :: fname = 'zmtxvis'

  ! Specify the action to be taken as a bitwise-OR of one _OP_, one _FN_, and one _FF_ flag.
  INTEGER, PARAMETER :: act = IOR(VN_CMPLXVIS_OP_A, VN_CMPLXVIS_FN_Id)

  ! Dimensions of A.
  INTEGER, PARAMETER :: mA = 200
  INTEGER, PARAMETER :: ldA = mA
  INTEGER, PARAMETER :: nA = 320

  ! Scaling factors.
  ! Each element of A will be represented by a rectangle of
  ! sx horizontal and sy vertical pixels in the bitmap.
  INTEGER, PARAMETER :: sx = 2
  INTEGER, PARAMETER :: sy = 2

  ! The input matrix.
  COMPLEX(c_double) :: A(ldA, nA)
  REAL(c_double) :: DRE, DIM

  ! A pointer to a context (see vn_cmplxvis.h).
  TYPE(c_ptr) :: ctx

  ! If < 0, info signals an error with the |info|-th parameter.
  ! If > 0, there was another kind of error (see the code in vn_cmplxvis.c).
  ! If = 0, there was no error (all OK).
  INTEGER :: info

  INTEGER :: I, J

  ! A "recording session" comprises of a sequence of frames that define the overall
  ! data range for colour-mapping.  There may be as many simultaneously active sessions
  ! as needed.  Each session should have a different fname, and thus consults its own
  ! palette, and produces its own dump file and bitmaps.

  ! Call (only!) once per a recording session, at the beginning.
  info = VN_CMPLXVIS_START(ctx, fname, act, mA, nA, sx, sy, LEN_TRIM(fname))
  IF (info .NE. 0) THEN
     WRITE (*,*) info
     STOP 'VN_CMPLXVIS_START'
  ELSE
     WRITE (*,*) 'Recording started.'
  END IF

  ! Let's fill in some values into A.
  ! The bitmap's upper-left corner will correspond to element A(1,1).
  DO I = 1, mA
     DO J = 1, nA
        DRE = REAL(MOD(I + J - 2, 254), c_double)
        DIM = REAL(MOD(I * J, 254), c_double)
        A(I, J) = CMPLX(DRE, DIM, c_double)
     END DO
  END DO

  ! Call as many times as needed, once per a frame to be recorded.
  ! Note that the leading dimension of A may vary form one call to another, but
  ! not the actual number of rows or columns.  Therefore, all the frames have to
  ! be of the same size!  Different-sized frames require different contexts.
  info = VN_CMPLXVIS_FRAME(ctx, A, ldA)
  IF (info .NE. 0) THEN
     WRITE (*,*) info
     STOP 'VN_CMPLXVIS_FRAME'
  ELSE
     WRITE (*,*) 'Recorded 1 frame.'
  END IF

  ! ...dump another frames...

  ! Call (only!) once per a recording session, at the end.
  info = VN_CMPLXVIS_STOP(ctx)
  IF (info .NE. 0) THEN
     WRITE (*,*) info
     STOP 'VN_CMPLXVIS_STOP'
  ELSE
     WRITE (*,*) 'Recording stopped.'
  END IF

END PROGRAM XZMTXVIS

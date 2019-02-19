! Build and run: please, see `xzmtxvis.sh`.
PROGRAM XZMTXVIS
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Please read ALL the comments below for details about the routines.

  INTERFACE
     FUNCTION VN_CMPLXVIS_START(ctx, fname, act, mA, nA, sx, sy, fname_len) BIND(C,name='vn_cmplxvis_start')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(OUT) :: ctx
       INTEGER, INTENT(IN), VALUE :: act, mA, nA, sx, sy, fname_len
       CHARACTER(c_char), INTENT(IN) :: fname(*)
       INTEGER :: VN_CMPLXVIS_START
     END FUNCTION VN_CMPLXVIS_START
  END INTERFACE

  INTERFACE
     FUNCTION VN_CMPLXVIS_FRAME(ctx, A, ldA) BIND(C,name='vn_cmplxvis_frame')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER, INTENT(IN), VALUE :: ldA
       COMPLEX(c_double), INTENT(IN) :: A(ldA,*)
       INTEGER :: VN_CMPLXVIS_FRAME
     END FUNCTION VN_CMPLXVIS_FRAME
  END INTERFACE

  INTERFACE
     FUNCTION VN_CMPLXVIS_STOP(ctx) BIND(C,name='vn_cmplxvis_stop')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER :: VN_CMPLXVIS_STOP
     END FUNCTION VN_CMPLXVIS_STOP
  END INTERFACE

  ! Let B = A (the input matrix).
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_A   =  0
  ! Let B = A^H.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_At  =  1
#ifndef VN_NO_BLAS
  ! Let B = A A^H.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_AAh =  2
  ! Let B = A^H A.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_AhA =  3
#endif

  ! Visualise the absolute value of B.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Id     =  0
  ! Visualise log_2 of the absolute value of B.
  ! If the input is zero or subnormal, take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Lg     =  4
  ! Visualise log_e of the absolute value of B.
  ! If the input is zero or subnormal, take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Ln     =  8
  ! Visualise log_10 of the absolute value of B.
  ! If the input is zero or subnormal take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Log    = 12
  ! Visualise the phase of B in [0, 2\pi] instead of [-\pi, \pi].
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Arg2PI = 16

  ! A temporary file is created to hold the frame dumps.

  ! Visualisation cannot proceed until the range of data in ALL frames is known,
  ! and is mapped to the chosen 256-colour (8-bit) palette.  The range and other
  ! needed metadata is written in the first 184 bytes of the dump file in ASCII
  ! form, so it is easily readable even when the rest of data is in binary form.

  ! With a given file prefix F, there has to be a palette file present in the
  ! current working directory, named F.plt and of the same form as examples in
  ! cmap/plt8 subdirectory here.  Each row contains three space-separated double
  ! precision values between 0 and 1, which are the amounts of red, green, and
  ! blue colour for the particular palette entry.  The examples provided are in
  ! principle the palettes available for the Matlab figures (jet, hsv, etc.).

  ! Palette colour 0 is reserved for negative infinites, and colour 255 for
  ! positive infinities and NaNs.  Apart from these special floating-point values,
  ! the data range is assumed to be finite, and is mapped to the remaining
  ! 254 palette colours (see normalise() function definition in vn_cmplxvis.c).

  ! The dump file is created in the current working directory, and is named
  ! F.dat.  It may be deleted once the bitmaps are created.

  ! The bitmaps for each frame are named FA%010d.bmp (the phase) and FR%010d.bmp
  ! (the absolute value), i.e., with a 0-padded 10-digit frame number.

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
     PRINT *, info
     STOP 'VN_CMPLXVIS_START'
  ELSE
     PRINT *, 'Recording started.'
  END IF

  ! Let's fill in some values into A.
  ! The bitmap's upper-left corner will correspond to element A(1,1).
  DO I = 1, mA
     DO J = 1, nA
        DRE = DBLE(MOD(I + J - 2, 254))
        DIM = DBLE(MOD(I * J, 254))
        A(I, J) = DCMPLX(DRE, DIM)
     END DO
  END DO

  ! Call as many times as needed, once per a frame to be recorded.
  ! Note that the leading dimension of A may vary form one call to another, but
  ! not the actual number of rows or columns.  Therefore, all the frames have to
  ! be of the same size!  Different-sized frames require different contexts.
  info = VN_CMPLXVIS_FRAME(ctx, A, ldA)
  IF (info .NE. 0) THEN
     PRINT *, info
     STOP 'VN_CMPLXVIS_FRAME'
  ELSE
     PRINT *, 'Recorded 1 frame.'
  END IF

  ! ...dump another frames...

  ! Call (only!) once per a recording session, at the end.
  info = VN_CMPLXVIS_STOP(ctx)
  IF (info .NE. 0) THEN
     PRINT *, info
     STOP 'VN_CMPLXVIS_STOP'
  ELSE
     PRINT *, 'Recording stopped.'
  END IF

END PROGRAM XZMTXVIS

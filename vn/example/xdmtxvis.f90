! Build: `ifort -mkl -standard-semantics xdmtxvis.f90 -o xdmtxvis.exe -L../.. -lvn`
! Don't forget to copy some palette file into dmtxvis.plt, as described below!
! E.g., `cp ../cmap/plt8/jet8.txt dmtxvis.plt` would do.
PROGRAM XDMTXVIS

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Please read ALL the comments below for details about the routines.

  INTERFACE
     FUNCTION VN_MTXVIS_START(ctx, fname, act, mA, nA, sx, sy, fname_len) BIND(C,name='vn_mtxvis_start')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(OUT) :: ctx
       INTEGER(c_int), INTENT(IN), VALUE :: act, mA, nA, sx, sy, fname_len
       CHARACTER(c_char), INTENT(IN) :: fname(*)
       INTEGER(c_int) :: VN_MTXVIS_START
     END FUNCTION VN_MTXVIS_START
  END INTERFACE

  INTERFACE
     FUNCTION VN_MTXVIS_FRAME(ctx, A, ldA) BIND(C,name='vn_mtxvis_frame')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER(c_int), INTENT(IN), VALUE :: ldA
       REAL(c_double), INTENT(IN) :: A(ldA,*)
       INTEGER(c_int) :: VN_MTXVIS_FRAME
     END FUNCTION VN_MTXVIS_FRAME
  END INTERFACE

  INTERFACE
     FUNCTION VN_MTXVIS_STOP(ctx) BIND(C,name='vn_mtxvis_stop')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER(c_int) :: VN_MTXVIS_STOP
     END FUNCTION VN_MTXVIS_STOP
  END INTERFACE

  ! Let B = A (the input matrix).
  INTEGER, PARAMETER :: VN_MTXVIS_OP_A   =  0
  ! Let B = A^T.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_At  =  1
  ! Let B = A A^T.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_AAt =  2
  ! Let B = A^T A.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_AtA =  3

  ! Visualise C = Id(B) (i.e., B).
  INTEGER, PARAMETER :: VN_MTXVIS_FN_Id  =  0
  ! Visualise C = |B| (C_{ij} = fabs(B_{ij})).
  INTEGER, PARAMETER :: VN_MTXVIS_FN_Abs =  4
  ! Visualise C = Lg(|B|) (C_{ij} = log2(fabs(B_{ij}))).
  ! If the input is zero or subnormal, set C_{ij} = -\infty.
  INTEGER, PARAMETER :: VN_MTXVIS_FN_Lg  =  8
  ! Visualise C = Log(|B|) (C_{ij} = log10(fabs(B_{ij}))).
  ! If the input is zero or subnormal, set C_{ij} = -\infty.
  INTEGER, PARAMETER :: VN_MTXVIS_FN_Log = 12

  ! A temporary file is created to hold the frame dumps.

  ! Visualisation cannot proceed until the range of data in ALL frames is known,
  ! and is mapped to the chosen 256-colour (8-bit) palette.  The range and other
  ! needed metadata is written in the first 128 bytes of the dump file in ASCII
  ! form, so it is easily readable even if the rest of data is in binary form.
  ! The file is not auto-deleted, so it can be used as a dumping/tracing facility
  ! in its own right, if the frames are written in ASCII form (25 characters per
  ! field, 17 digits after decimal point, one space as the field separator, each
  ! matrix row in its own line), or if read from the binary form into memory.

  ! Create a temporary file containing the frame dumps in binary (machine) form.
  INTEGER, PARAMETER :: VN_MTXVIS_FF_Bin =  0
  ! Create a temporary file containing the frame dumps in ASCII (text) form.
  INTEGER, PARAMETER :: VN_MTXVIS_FF_Asc = 16

  ! With a given file prefix F, there has to be a palette file present in the
  ! current working directory, named F.plt and of the same form as examples in
  ! cmap/plt8 subdirectory here.  Each row contains three space-separated double
  ! precision values between 0 and 1, which are the amounts of red, green, and
  ! blue colour for the particular palette entry.  The examples provided are in
  ! principle the palettes available for the Matlab figures (jet, hsv, etc.).

  ! Palette colour 0 is reserved for negative infinites, and colour 255 for
  ! positive infinities and NaNs.  Apart from these special floating-point values,
  ! the data range is assumed to be finite, and is mapped to the remaining
  ! 254 palette colours (see normalise() function definition in vn_mtxvis.c).

  ! The dump file is created in the current working directory, and is named
  ! F.txt or F.dat, depending on whether the frames are written in ASCII or
  ! binary form, respectively.  It may be deleted once the bitmaps are created.

  ! The bitmaps are named F-%010d.bmp, i.e., with 0-padded 10-digit frame number.

  ! File name prefix of length at most 7 for the dump, palette, and bitmap files.
  CHARACTER(LEN=7), PARAMETER :: fname = 'dmtxvis'

  ! Specify the action to be taken as a bitwise-OR of one _OP_, one _FN_, and one _FF_ flag.
  INTEGER, PARAMETER :: act = IOR(IOR(VN_MTXVIS_OP_A, VN_MTXVIS_FN_Id), VN_MTXVIS_FF_Bin)

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
  DOUBLE PRECISION :: A(ldA, nA)

  ! A pointer to a context (see vn_mtxvis.h).
  TYPE(c_ptr) :: ctx

  ! If < 0, info signals an error with the |info|-th parameter.
  ! If > 0, there was another kind of error (see the code in vn_mtxvis.c).
  ! If = 0, there was no error (all OK).
  INTEGER :: info

  INTEGER :: I, J

  ! A "recording session" comprises of a sequence of frames that define the overall
  ! data range for colour-mapping.  There may be as many simultaneously active sessions
  ! as needed.  Each session should have a different fname, and thus consults its own
  ! palette, and produces its own dump file and bitmaps.

  ! Call (only!) once per a recording session, at the beginning.
  info = VN_MTXVIS_START(ctx, fname, act, mA, nA, sx, sy, LEN_TRIM(fname))
  IF (info .NE. 0) THEN
     PRINT *, info
     STOP 'VN_MTXVIS_START'
  ELSE
     PRINT *, 'Recording started.'
  END IF

  ! Let's fill in some values into A.
  ! The bitmap's upper-left corner will correspond to element A(1,1).
  DO I = 1, mA
     DO J = 1, nA
        A(I, J) = DBLE(MOD(I + J - 2, 254))
     END DO
  END DO

  ! Call as many times as needed, once per a frame to be recorded.
  ! Note that the leading dimension of A may vary form one call to another, but
  ! not the actual number of rows or columns.  Therefore, all the frames have to
  ! be of the same size!  Different-sized frames require different contexts.
  info = VN_MTXVIS_FRAME(ctx, A, ldA)
  IF (info .NE. 0) THEN
     PRINT *, info
     STOP 'VN_MTXVIS_FRAME'
  ELSE
     PRINT *, 'Recorded 1 frame.'
  END IF

  ! ...dump another frames...

  ! Call (only!) once per a recording session, at the end.
  info = VN_MTXVIS_STOP(ctx)
  IF (info .NE. 0) THEN
     PRINT *, info
     STOP 'VN_MTXVIS_STOP'
  ELSE
     PRINT *, 'Recording stopped.'
  END IF

END PROGRAM XDMTXVIS

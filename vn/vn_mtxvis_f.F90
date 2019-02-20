MODULE VN_MTXVIS_F
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Please read ALL the comments below for details about the routines.

  INTERFACE
     FUNCTION VN_MTXVIS_START(ctx, fname, act, mA, nA, sx, sy, fname_len) BIND(C,name='vn_mtxvis_start')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(OUT) :: ctx
       INTEGER, INTENT(IN), VALUE :: act, mA, nA, sx, sy, fname_len
       CHARACTER(c_char), INTENT(IN) :: fname(*)
       INTEGER :: VN_MTXVIS_START
     END FUNCTION VN_MTXVIS_START
  END INTERFACE

  INTERFACE
     FUNCTION VN_MTXVIS_FRAME(ctx, A, ldA) BIND(C,name='vn_mtxvis_frame')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER, INTENT(IN), VALUE :: ldA
       REAL(c_double), INTENT(IN) :: A(ldA,*)
       INTEGER :: VN_MTXVIS_FRAME
     END FUNCTION VN_MTXVIS_FRAME
  END INTERFACE

  INTERFACE
     FUNCTION VN_MTXVIS_STOP(ctx) BIND(C,name='vn_mtxvis_stop')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER :: VN_MTXVIS_STOP
     END FUNCTION VN_MTXVIS_STOP
  END INTERFACE

  ! Let B = A (the input matrix).
  INTEGER, PARAMETER :: VN_MTXVIS_OP_A   =  0
  ! Let B = A^T.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_At  =  1
#ifndef VN_NO_BLAS
  ! Let B = A A^T.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_AAt =  2
  ! Let B = A^T A.
  INTEGER, PARAMETER :: VN_MTXVIS_OP_AtA =  3
#endif

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

CONTAINS
END MODULE VN_MTXVIS_F
